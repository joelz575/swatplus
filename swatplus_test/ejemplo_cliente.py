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
