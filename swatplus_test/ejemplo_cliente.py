import json
import socket
import sys
import time
from struct import unpack, pack

import numpy as np

datos = {
    'entero': np.arange(5, dtype=int),
    'decimal': np.arange(0, 5, 0.5),
    'entero negativo': np.arange(-5, 5, dtype=int),
    'decimal negativo': np.arange(-5, 5, 0.5)
}
leer_datos = {
    'sd_props': np.array([0, 0]),
    'sd_obj_no': np.array([0, 0]),
    'sd_aqu_link': np.array([0, 0]),       #aquifer the channel is linked to
    'sd_aqu_link_ch' : np.array([0, 0]),   #sequential channel number in the aquifer
    'sd_chw' : np.array([3.0, 8.37300014]),       #m          |channel width
    'sd_chd': np.array([0.5, 3.79999995]),        #m          |channel depth
    'sd_chs': np.array([9.99999978E-03, 2.30999999E-02]),        #m/m        |channel slope
    'sd_chl': np.array([0.100000001, 0.229000002]),        #km         |channel length
    'sd_chn': np.array([1.79366203E-43, 5.00000007E-02]),        #           |channel Manning's n
    'sd_cov': np.array([0.0, 5.00000007E-02]),        #0-1        |channel cover factor
    'sd_cherod': np.array([float('nan'), 0.200000003]),     #           |channel erodibility
    'sd_shear_bnk': np.array([float('nan'), float('nan')]),  #0-1        |bank shear coefficient - fraction of bottom shear
    'sd_hc_erod': np.array([1.40129846E-45, 1.40129846E-45]),    #           |headcut erodibility
    'sd_hc_co': np.array([0.0, 7.47151327]),      #m/m        |proportionality coefficient for head cut
    'sd_hc_len': np.array([0.0, 0.0]),     #m          |length of head cut
    'sd_hc_hgt': np.array([6.86636248E-44, 1.0]),     #m          |headcut height
    'sd_stor': np.array([0.0, 0.0]),       #m3         |water stored in reach at end of the day
    #'sd_kd' : np.array([1.0, 1.0]),        #           |aquatic mixing velocity (diffusion/dispersion)-using mol_wt
    #'sd_aq_mix' : np.array([1.0, 1.0])     # m/day     |aquatic mixing velocity (diffusion/dispersion)-using mol_wt
    #'name': ,
    'lte_props': np.array([0]),
    'lte_obj_no': np.array([0]),
    #character(len=16) :: lsu             #              |landscape unit - character
    #character(len=16) :: region          #              |region - character
    #character(len=16) :: plant           #              |plant type (as listed in plants.plt)
    'lte_iplant': np.array([0]),                  #              |plant number xwalked from hlt_db()%plant and plants.plt
    'lte_km2': np.array([0.0]),                     #km^2          |drainage area
    'lte_cn2': np.array([0.0]),                     #              |condition II curve number (used in calibration)
    'lte_cn3_swf': np.array([0.0]),                #none          |soil water factor for cn3 (used in calibration)
    #              |0 = fc; 1 = saturation (porosity)
    'lte_soildep': np.array([0.0]),                #mm            |soil profile depth
    'lte_etco': np.array([0.0]),                    #              |et coefficient - use with pet and aet (used in calibration)
    'lte_revapc': np.array([0.0]),                  #m/m           |revap from aquifer (used in calibration)
    'lte_perco': np.array([0.0]),                  #              |soil percolation coefficient (used in calibration)
    'lte_tdrain': np.array([0.0]),                  #hr            |design subsurface tile drain time (used in calibration)
    'lte_stress': np.array([0.0]),                  #frac          |plant stress - pest, root restriction, soil quality, nutrient,
    #              |(non water, temp) (used in calibration)
    'lte_uslefac': np.array([0.0]),                 #              |USLE slope length factor
    'lte_wrt1': np.array([0.0]),
    'lte_wrt2': np.array([0.0]),
    'lte_smx': np.array([0.0]),
    'lte_hk': np.array([0.0]),
    'lte_yls': np.array([0.0]),
    'lte_ylc': np.array([0.0]),
    'lte_awc': np.array([0.0]),                    #mm/mm        |available water capacity of soil
    'lte_g': np.array([0.0]),
    'lte_hufh': np.array([0.0]),
    'lte_phu': np.array([0.0]),
    'lte_por': np.array([0.0]),
    'lte_sc': np.array([0.0]),
    'lte_sw': np.array([0.0]),                      #mm/mm         |initial soil water storage
    'lte_gw': np.array([0.0]),                      #mm            |initial shallow aquifer storage
    'lte_snow': np.array([0.0]),                    #mm            |initial water content of snow
    'lte_gwflow': np.array([0.0]),                  #mm            |initial groundwater flow
    #character(len=1) :: gro = "n"        #              |y=plant growing; n=not growing;
    'lte_dm': np.array([0.0]),                     #t/ha          |plant biomass
    'lte_alai': np.array([0.0]),                    #              |leaf area index
    'lte_yield': np.array([0.0]),                   #t/ha          |plant yield
    'lte_npp': np.array([0.0]),                     #t/ha          |net primary productivity
    'lte_lai_mx': np.array([0.0]),                 #              |maximum leaf area index
    'lte_gwdeep': np.array([0.0]),                 #mm            |deep aquifer storage
    'lte_aet': np.array([0.0]),                     #mm            |sum of actual et during growing season (for hi water stress)
    'lte_pet': np.array([0.0]),                     #mm            |sum of potential et during growing season (for hi water stress)
    'lte_start': np.array([0]),
    'lte_end': np.array([0]),
}

def cliente_puertos(dirección, puerto, t_final):
    t_final = int(t_final)
    vals = datos.copy()
    vals['t'] = np.array([0])
    with socket.socket(socket.AF_INET, socket.SOCK_STREAM) as e:
        e.connect((dirección, int(puerto)))

        while True:
            encab = _recibir_encabezado(e)
            tipo = encab['tipo'].lower()

            if tipo == 'incr':
                n_pasos = encab['n_pasos']
                if n_pasos == 0:
                    vals['t'][:] = t_final
                else:
                    vals['t'][:] = min(vals['t'] + n_pasos, t_final)

            elif tipo == 'leer':
                var = encab['var']

                vals_bits = vals[var].tobytes()
                encabezado = json.dumps({
                    'tamaño': len(vals_bits),
                    'tipo_cont': str(vals[var].dtype),
                    'forma': vals[var].shape
                }).encode('utf8')
                e.sendall(pack('i', len(encabezado)))
                e.sendall(encabezado)

                e.sendall(vals_bits)

            elif tipo == 'cambiar':
                var = encab['var']
                tmñ = encab['tamaño']
                tipo_m = encab['tipo_cont']
                forma = encab['forma']

                val = np.frombuffer(e.recv(tmñ), dtype=tipo_m).reshape(forma)
                vals[var] = val

            elif tipo == 'cerrar':
                break

            else:
                raise ValueError(tipo)


def _recibir_encabezado(e):
    tmñ = unpack('i', e.recv(4))[0]
    return json.loads(e.recv(tmñ).decode('utf8'))


if __name__ == '__main__':
    cliente_puertos(*sys.argv[1:])
