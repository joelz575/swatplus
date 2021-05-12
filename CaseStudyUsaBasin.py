import os
import tracemalloc
from subprocess import Popen
from unittest import TestCase
import numpy as np
import numpy.testing as npt
from tinamit.idm.puertos import IDMEnchufes

case_study_test_data = {
    'sd_props': np.array([0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]),
    'sd_aqu_link_ch': np.array([0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]),   #sequential channel number in the aquifer
    'sd_chw': np.array([3.0, 17.89411, 13.49788, 11.25827,  9.20389, 7.34176,
                         10.84738, 17.31699,  9.07698, 33.87193, 23.19207, 21.2029]),       #m          |channel width
    'sd_chd': np.array([0.5, 0.75051, 0.62191, 0.55106, 0.4818, 0.4144, 0.53757,
       0.73429, 0.47736, 1.14844, 0.89216, 0.84039])  # m          |channel depth
}
t_final = 4018
valgrind = False
BASE_DIR = os.path.split(os.path.split(__file__)[0])[0]


class PruebaIDM(TestCase):

    def setUp(símismo):
        símismo.clientes = []

    def _empezar_cliente(símismo, dirección, puerto):
        cwd = "/home/joelz/Documents/Prof Adamowski/CaseStudyModel/UsaSWAT+Model/usa_model/usa_model/" \
              "usa_model_update/Scenarios/Default/TxtInOut"
        swat_exe = os.path.join(BASE_DIR, "swatplus/build/bin/swatplus_exe")
        if valgrind:
            cliente = Popen(["valgrind", "--leak-check=yes", "--track-origins=yes", swat_exe, str(puerto), dirección],
                            cwd=cwd)
        else:
            cliente = Popen([swat_exe, str(puerto), dirección],
                            cwd=cwd)

        símismo.clientes.append(cliente)
        return cliente

    def test_abrir_cerrar(símismo):
        with IDMEnchufes() as servidor:
            símismo._empezar_cliente(servidor.dirección, servidor.puerto)
            servidor.activar()
            servidor.cerrar()

    def test_ciclo_completo(símismo):
        npasos = 5
        t = 0
        #tracemalloc.start()
        for nmbr_dts, dts in case_study_test_data.items():
            check_t = 0
            with símismo.subTest(datos=nmbr_dts), IDMEnchufes() as servidor:
                símismo._empezar_cliente(servidor.dirección, servidor.puerto)
                servidor.activar()
                while t < 20:
                    recibido = servidor.recibir(nmbr_dts)
                    npt.assert_almost_equal(dts, recibido, 5)
                    # equal to 5 decimal places
                    dts = dts+1
                    print("going to send this data: ", dts)
                    servidor.cambiar(nmbr_dts, dts)
                    servidor.incrementar(npasos)
                    t = servidor.recibir('t')
                    check_t = check_t + npasos
                    npt.assert_equal(check_t, t)