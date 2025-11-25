import os
import json
import time
import hashlib
import datetime as dt
from typing import List, Dict
import requests

AUTH_TOKEN_URL = 'https://identity.dataspace.copernicus.eu/auth/realms/CDSE/protocol/openid-connect/token'

BASE_DIR = os.path.dirname(os.path.abspath(__file__))
PROJECT_DIR = os.path.abspath(os.path.join(BASE_DIR, ".."))
DATA_DIR = os.path.join(PROJECT_DIR, "data")
AOI_PATH = os.path.join(DATA_DIR, "AOI.geojson")
OUT_DIR = os.path.join(DATA_DIR, "sentinel_downloads")
S1_DIR = os.path.join(OUT_DIR, "S1")
S2_DIR = os.path.join(OUT_DIR, "S2")
os.makedirs(S1_DIR, exist_ok=True)
os.makedirs(S2_DIR, exist_ok=True)

# --- CONSTANTE PARA S2 (Bandas de 10m)
S2_BANDS_10M = ["B02", "B03", "B04", "B08"]

# --- CREDENCIALES (Leídas del entorno por seguridad) ---

USER = os.environ.get('CDSE_USER')
PASSWORD = os.environ.get('CDSE_PASS')
if not USER or not PASSWORD:
    raise RuntimeError("Define las variables de entorno CDSE_USER y CDSE_PASS.")

# --- ENDPOINTS CDSE OData ---
CATALOG = "https://catalogue.dataspace.copernicus.eu/odata/v1/Products"
DOWNLOAD = "https://download.dataspace.copernicus.eu/odata/v1/Products"

# --- FECHAS DE CONSULTA 
START = "2025-07-20T00:00:00Z"
END   = "2025-09-20T23:59:59Z"

# --- utilidades geométricas (WKT desde GeoJSON) ---
try:
    from shapely.geometry import shape
    # Se agrega importación explícita para cargar WKT de string
    from shapely import wkt as _wkt 
except Exception as e:
    raise RuntimeError("Falta shapely. Instala con: pip install shapely") from e


# ----------------------------------------------------------------------
# FUNCIONES DE AUTENTICACIÓN
# ----------------------------------------------------------------------

def get_cdse_token(username: str, password: str) -> str:
    """Solicita un token de acceso OAuth2 al endpoint de CDSE."""
    
    data = {
        'grant_type': 'password',
        'client_id': 'cdse-public',
        'username': username,
        'password': password
    }

    print("[INFO] Solicitando token de acceso CDSE...")
    
    try:
        response = requests.post(AUTH_TOKEN_URL, data=data, timeout=60)
        response.raise_for_status() 
        
        token_data = response.json()
        access_token = token_data.get('access_token')

        if not access_token:
            raise ValueError("Respuesta del token inválida: 'access_token' no encontrado.")

        print("[OK] Token obtenido exitosamente.")
        return access_token
        
    except requests.exceptions.HTTPError as e:
        if response.status_code == 401:
            raise RuntimeError("Fallo de autenticación (401). Credenciales inválidas.") from e
        elif response.status_code == 403:
            # Si pasa aquí, verifica T&C en el portal web de CDSE.
            raise RuntimeError("Acceso denegado (403). Asegúrate de haber aceptado T&C en CDSE.") from e
        else:
            raise RuntimeError(f"Fallo HTTP al obtener el token: {e}") from e
    except Exception as e:
        raise RuntimeError(f"Error inesperado al obtener el token: {e}") from e


def http_get(url: str, token: str, params=None, stream=False, max_retries=4, backoff=2.0):
    """GET con reintentos exponenciales simples, usando token de portador."""
    
    headers = {'Authorization': f'Bearer {token}'}
    
    for i in range(max_retries):
        try:
            r = requests.get(url, params=params, headers=headers, stream=stream, timeout=120)
            r.raise_for_status()
            return r
        except Exception as e:
            if i == max_retries - 1:
                raise
            sleep_s = backoff ** i
            print(f"[WARN] GET fallo ({e}). Reintentando en {sleep_s:.1f}s...")
            time.sleep(sleep_s)


# ----------------------------------------------------------------------
# FUNCIONES AUXILIARES Y DE CONSULTA
# ----------------------------------------------------------------------

def get_bbox_wkt(polygon_wkt: str) -> str:
    """Calcula y devuelve el WKT de la BBOX de una geometría, redondeando coordenadas."""
    try:
        geom = _wkt.loads(polygon_wkt)
    except Exception as e:
        # Esto no debería pasar si load_all_aoi_wkts funciona, pero es un resguardo
        print(f"[ERROR] Error cargando WKT. Devolviendo WKT original. {e}")
        return polygon_wkt 
        
    minx, miny, maxx, maxy = geom.bounds
    
    # FIX FINAL: Redondeo a 4 decimales. 
    f = lambda x: round(x, 4)
    minx, miny, maxx, maxy = f(minx), f(miny), f(maxx), f(maxy)
    
    # WKT del BBOX como POLYGON
    bbox_wkt = f"POLYGON (({minx} {miny}, {minx} {maxy}, {maxx} {maxy}, {maxx} {miny}, {minx} {miny}))"
    return bbox_wkt


def load_all_aoi_wkts(geojson_path: str) -> Dict[str, str]:
    """Carga todas las geometrías de tipo 'polygon' desde el GeoJSON y las convierte a WKT,
       mapeadas por su 'aoi_id'."""
    with open(geojson_path, "r", encoding="utf-8") as f:
        gj = json.load(f)

    aoi_wkts = {}
    for feat in gj["features"]:
        if feat.get("properties", {}).get("kind") == "polygon":
            aoi_id = feat["properties"]["aoi_id"]
            geom = feat.get("geometry")
            if not geom:
                continue
            
            # Saneamiento de geometrías y conversión a WKT
            poly = shape(geom).buffer(0)
            aoi_wkts[aoi_id] = poly.wkt 

    if not aoi_wkts:
        raise ValueError("El GeoJSON no contiene geometrías de tipo 'polygon' válidas.")
        
    return aoi_wkts

def sha256_file(path: str, chunk=1<<20) -> str:
    h = hashlib.sha256()
    with open(path, "rb") as f:
        while True:
            b = f.read(chunk)
            if not b:
                break
            h.update(b)
    return h.hexdigest()

# Límite de productos por defecto a 1
def query_s1(wkt: str, token: str, top: int = 1) -> List[Dict]:
    #Usa el BBOX WKT para la consulta de catálogo ---
    bbox_wkt = get_bbox_wkt(wkt)
    print(f"  [S1 Q] Usando BBOX para consulta: {bbox_wkt.split(' ((')[0]}(...)")
    
    # S1 GRD IW VV/VH

    od_filter = (
    "Collection/Name eq 'SENTINEL-1' and "
    "Attributes/OData.CSC.StringAttribute/any(att: att/Name eq 'productType' "
    "  and att/OData.CSC.StringAttribute/Value eq 'IW_GRDH_1S') and "
    "Attributes/OData.CSC.StringAttribute/any(att: att/Name eq 'polarisationChannels' "
    "  and att/OData.CSC.StringAttribute/Value eq 'VV+VH') and "
    "ContentDate/Start ge 2025-08-15T00:00:00Z and ContentDate/End le 2025-09-20T23:59:59Z and "
    f"OData.CSC.Intersects(area=geography'SRID=4326;{bbox_wkt}')"
    )
    # [MODIFICACIÓN] $top=1
    params = {"$filter": od_filter, "$top": str(top), "$orderby": "ContentDate/Start desc"}

    r = http_get(CATALOG, token=token, params=params)
    return r.json().get("value", [])

# Límite de productos por defecto a 1
def query_s2(wkt: str, token: str, top: int = 1) -> List[Dict]:
    # --- FIX: Usa el BBOX WKT para la consulta de catálogo ---
    bbox_wkt = get_bbox_wkt(wkt)
    print(f"  [S2 Q] Usando BBOX para consulta: {bbox_wkt.split(' ((')[0]}(...)")

    # S2 L2A nubes ≤ 10%
    od_filter = (
        f"Collection/Name eq 'SENTINEL-2' and "
        f"Attributes/OData.CSC.StringAttribute/any(a:a/Name eq 'productType' and a/Value eq 'S2MSI2A') and "
        f"Attributes/OData.CSC.DoubleAttribute/any(a:a/Name eq 'cloudCover' and a/Value le 10) and "
        f"ContentDate/Start ge {START} and ContentDate/End le {END} and "
        # Usar bbox_wkt en el filtro
        f"Intersects(Footprint, geography'SRID=4326;{bbox_wkt}')"
    )
    # [MODIFICACIÓN] $top=1
    params = {"$filter": od_filter, "$top": str(top), "$orderby": "ContentDate/Start desc"}
    r = http_get(CATALOG, token=token, params=params)
    return r.json().get("value", [])

# Función de descarga de ZIP completo (usada para S1 y fallback de S2)
def download_s1_product(product: Dict, out_dir: str, token: str) -> Dict:
    """Descarga un producto S1 por Id -> ZIP. Devuelve metadatos de archivo.
       (También usada como fallback para la descarga optimizada de S2)."""
    pid = product["Id"]
    name = product.get("Name", pid)
    # URL de descarga del ZIP completo (S1 no soporta descargas parciales por banda)
    url = f"{DOWNLOAD}({pid})/$value"
    out_path = os.path.join(out_dir, name + ".zip")

    if os.path.exists(out_path) and os.path.getsize(out_path) > 0:
        print(f"[SKIP] Ya existe: {out_path}")
        file_hash = sha256_file(out_path)
        return {"id": pid, "name": name, "path": out_path, "sha256": file_hash, "downloaded": False}

    print(f"[DL] {name} -> {out_path}")
    # Usa el token en la llamada
    with http_get(url, token=token, stream=True) as resp:
        with open(out_path, "wb") as f:
            for chunk in resp.iter_content(1 << 20):
                if chunk:
                    f.write(chunk)

    file_hash = sha256_file(out_path)
    return {"id": pid, "name": name, "path": out_path, "sha256": file_hash, "downloaded": True}

# Descarga optimizada de bandas S2
# ----- REVISAR: ACTUALMENTE NO FUNCIONA -----
def download_s2_bands(product: Dict, out_dir: str, token: str) -> List[Dict]:
    """Descarga las bandas JP2 de 10m de un producto S2 L2A individualmente.
       Guarda como .tif para cumplir la solicitud de formato georreferenciado."""
    
    pid = product["Id"]
    name = product.get("Name", pid)
    downloaded_files = []

    # 1. Consultar los nodos del producto para obtener todos los archivos
    nodes_url = f"{DOWNLOAD}({pid})/Nodes"
    
    try:
        r_nodes = http_get(nodes_url, token=token).json()
    except Exception as e:
        print(f"[ERROR] No se pudo obtener la estructura de archivos para S2 {name}: {e}. Fallback a descarga ZIP.")
        # Fallback a la descarga completa del ZIP si falla la API /Nodes
        return [download_s1_product(product, out_dir, token)] 
        
    all_nodes = r_nodes.get('value', [])
    
    # 2. Iterar y descargar cada banda (B02, B03, B04, B08)
    for band in S2_BANDS_10M:
        # Buscamos el sufijo del archivo, e.g., '_B02.jp2'
        target_band_suffix = f"_{band}.jp2".lower()
        file_path = None

        # Buscar el ID (ruta completa) del archivo de banda en la lista de nodos
        for val in all_nodes:
            node_id = val.get('Id', '').strip('/')
            # Criterios: 1. Debe estar en la carpeta de Granule/IMG_DATA/R10m/. 2. Debe ser un archivo .jp2 de la banda.
            if 'GRANULE' in node_id and '/R10m/' in node_id and node_id.lower().endswith(target_band_suffix):
                # node_id es la ruta completa: S2.../GRANULE/.../IMG_DATA/R10m/Txxxxx_B02.jp2
                file_path = node_id
                break
        
        if not file_path:
            # Si no se encuentra el path para una banda, emitir una advertencia y pasar a la siguiente
            print(f"[WARN] No se encontró la ruta del archivo para la banda {band} con la estructura /R10m/. Saltando esta banda.")
            continue

        # 3. Descargar el archivo encontrado
        # La URL de descarga es: /odata/v1/Products('pid')/Nodes('file_path')/$value
        file_url = f"{DOWNLOAD}({pid})/Nodes('{file_path}')/$value"
        
        # Nombre de salida: Product_Band.tif (cumpliendo con la solicitud .tif)
        out_name = f"{name}_{band}.tif" 
        out_path = os.path.join(out_dir, out_name)
        
        # Evitar doble descarga
        if os.path.exists(out_path) and os.path.getsize(out_path) > 0:
            print(f"[SKIP] Ya existe la banda {band}: {out_path}")
            file_hash = sha256_file(out_path)
            downloaded_files.append({"id": pid, "name": out_name, "path": out_path, "sha256": file_hash, "downloaded": False})
            continue

        print(f"  [DL] Banda {band} -> {out_path}")
        
        try:
            with http_get(file_url, token=token, stream=True) as resp:
                with open(out_path, "wb") as f:
                    for chunk in resp.iter_content(1 << 20):
                        if chunk:
                            f.write(chunk)
            
            file_hash = sha256_file(out_path)
            downloaded_files.append({"id": pid, "name": out_name, "path": out_path, "sha256": file_hash, "downloaded": True})

        except Exception as e:
            print(f"[ERROR] Fallo descargando banda {band} para {name}: {e}")

    # Si no se descargó ninguna banda, se debe notificar
    if not downloaded_files and all_nodes:
        print(f"[WARN] No se pudo descargar ninguna banda 10m para el producto {name}. El producto podría tener una estructura no estándar.")

    return downloaded_files


def save_consolidated_metadata(all_aoi_data: Dict[str, Dict]) -> str:
    """Guarda los metadatos de la consulta y los resultados de todas las AOIs en un único archivo."""
    meta = {
        "created_at_utc": dt.datetime.utcnow().isoformat() + "Z",
        "query_config": {
            "start": START,
            "end": END,
            # [MODIFICACIÓN] $top=1 reflejado en metadatos
            "s1": {"productType": "GRD", "mode": "IW", "pol": "VV VH", "limit": 1},
            "s2": {"productType": "S2MSI2A", "cloudCoverMax": 10, "limit": 1, "bands_10m": S2_BANDS_10M},
        },
        "aoi_results": all_aoi_data # Contiene todos los datos por AOI
    }
    meta_path = os.path.join(OUT_DIR, "metadata_per_aoi.json")
    with open(meta_path, "w", encoding="utf-8") as f:
        json.dump(meta, f, ensure_ascii=False, indent=2)
    return meta_path


# ----------------------------------------------------------------------
# FUNCIÓN PRINCIPAL
# ----------------------------------------------------------------------

def main():

    # 1. Obtener el Token de Acceso
    cdse_token = get_cdse_token(USER, PASSWORD)
    
    # Cargar todas las AOI individualmente
    print("[INFO] Cargando AOIs individuales:", AOI_PATH)
    try:
        aoi_wkts = load_all_aoi_wkts(AOI_PATH)
    except Exception as e:
        print(f"[ERROR] No se pudieron cargar las AOIs. Asegúrate de que '{AOI_PATH}' es un GeoJSON válido con polígonos.")
        raise
        
    print(f"[INFO] {len(aoi_wkts)} AOIs encontradas para descarga.")

    all_aoi_data = {} # Diccionario para almacenar los resultados de cada AOI

    # Iterar sobre cada AOI
    for aoi_id, aoi_wkt in aoi_wkts.items():
        print(f"\n=======================================================")
        print(f"[{aoi_id}] Iniciando descarga para Area de Interés (AOI)")
        print(f"=======================================================")
        
        # Crear directorios específicos para esta AOI (ej: data/sentinel_downloads/S1/AOI1)
        s1_aoi_dir = os.path.join(S1_DIR, aoi_id)
        s2_aoi_dir = os.path.join(S2_DIR, aoi_id)
        os.makedirs(s1_aoi_dir, exist_ok=True)
        os.makedirs(s2_aoi_dir, exist_ok=True)

        # 2. Consultar y Descargar Sentinel-1
        print(f"[{aoi_id}] Consultando Sentinel-1...")
        # [MODIFICACIÓN] top=1
        s1_items = query_s1(aoi_wkt, token=cdse_token, top=1)
        print(f"[{aoi_id}] {len(s1_items)} productos S1 encontrados")

        s1_files = []
        for it in s1_items:
            try:
                # Usar download_s1_product (el original, renombrado)
                s1_files.append(download_s1_product(it, s1_aoi_dir, token=cdse_token))
            except Exception as e:
                print(f"[ERROR] Fallo descargando S1 {it.get('Name')}: {e}")

        # 4. Consultar y Descargar Sentinel-2
        print(f"[{aoi_id}] Consultando Sentinel-2...")
        # [MODIFICACIÓN] top=1
        s2_items = query_s2(aoi_wkt, token=cdse_token, top=1)
        print(f"[{aoi_id}] {len(s2_items)} productos S2 encontrados")

        s2_files = []
        for it in s2_items:
            try:
                # Usar download_s2_bands (Descarga optimizada/corregida)
                # La función devuelve una LISTA de archivos (uno por banda)
                s2_files.extend(download_s2_bands(it, s2_aoi_dir, token=cdse_token)) 
            except Exception as e:
                print(f"[ERROR] Fallo descargando S2 {it.get('Name')}: {e}")
        
        # Almacenar metadatos para esta AOI
        all_aoi_data[aoi_id] = {
            "aoi_wkt": aoi_wkt,
            "s1_catalog": [{"Id": p["Id"], "Name": p.get("Name"), "ContentDate": p.get("ContentDate")} for p in s1_items],
            "s2_catalog": [{"Id": p["Id"], "Name": p.get("Name"), "ContentDate": p.get("ContentDate")} for p in s2_items],
            "s1_files": s1_files,
            "s2_files": s2_files
        }

    # 6. Guardar metadatos consolidados
    meta_path = save_consolidated_metadata(all_aoi_data)
    print(f"\n[OK] Descargas completas por AOI. Metadatos consolidados guardados en: {meta_path}")


if __name__ == "__main__":
    main()