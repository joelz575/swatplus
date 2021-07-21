import os
from subprocess import Popen
from unittest import TestCase
import numpy.testing as npt
from tinamit.idm.puertos import IDMEnchufes

from swatplus_test.ejemplo_cliente import test_datos, datos

t_final = [3287, 3287, 731, 4018]
valgrind = False
BASE_DIR = os.path.split(os.path.split(__file__)[0])[0]
testDirectories = [
                   #"07120002_Iroquois_IL", #Skipped because it fails with only the SWAT+ source code already
                   "ceap_connectivity test",
                   #"Chris_George_tx",      #Skipped because the model is too large
                   #"Little_River_Tifton",  #Skipped because it fails with only the SWAT+ source code already
                   "saturated_buffer",
                   "Texas_large_gully",
                   #"tropic_dataset",       #Skipped because it fails with only the SWAT+ source code already
                   #"TxtInOut_CoonCreek_aqu", # does not run on travis.yml for some reason, number of days is 366
                   "Usa_Basin_model"
                   ]

class PruebaIDM(TestCase):

    def setUp(símismo):
        símismo.clientes = []

    def _empezar_cliente(símismo, dirección, puerto, n):
        cwd = os.path.join(BASE_DIR, "swatplus_test/" + testDirectories[n])
        swat_exe = os.path.join(BASE_DIR, "build/bin/swatplus_exe")

        if valgrind:
            cliente = Popen(["valgrind", "--leak-check=yes", "--track-origins=yes", swat_exe, str(puerto), dirección],
                            cwd=cwd)
        else:
            cliente = Popen([swat_exe, str(puerto), dirección], cwd=cwd)

        símismo.clientes.append(cliente)
        return cliente

    def test_abrir_cerrar(símismo):
        for n in range(len(testDirectories)):
            with símismo.subTest(datos=testDirectories[n]), IDMEnchufes() as servidor:
                símismo._empezar_cliente(servidor.dirección, servidor.puerto,n)
                servidor.activar()
                servidor.cerrar()

    def test_mandar_datos(símismo):
        for n in range(len(testDirectories)):
            if test_datos[testDirectories[n]] != "":
                for nmbr_dts in test_datos.get(testDirectories[n]):
                    dts = datos[nmbr_dts]
                    with símismo.subTest(datos=testDirectories[n]+"/"+nmbr_dts), IDMEnchufes() as servidor:
                        símismo._empezar_cliente(servidor.dirección, servidor.puerto, n)
                        servidor.activar()
                        print("going to send this data: ", dts)
                        servidor.cambiar(nmbr_dts, dts)
                        servidor.cambiar(nmbr_dts, dts)
                        recibido = servidor.recibir(nmbr_dts)
                        # npt.assert_almost_equal(dts, recibido)
                        # equal to 7 decimal places
                        npt.assert_almost_equal(dts, recibido, 5)
                        # equal to 5 decimal places

    # def test_recibir_datos(símismo):
    #     for n in range(len(testDirectories)):
    #         if test_datos[testDirectories[n]] != "":
    #             for nmbr_dts in test_datos.get(testDirectories[n]):
    #                 dts = datos[nmbr_dts]
    #                 with símismo.subTest(datos=testDirectories[n] + "/" + nmbr_dts), IDMEnchufes() as servidor:
    #                     símismo._empezar_cliente(servidor.dirección, servidor.puerto, n)
    #                     servidor.activar()
    #                     recibido = servidor.recibir(nmbr_dts)
    #                     print("receiving ", nmbr_dts)
    #                     print("received ", recibido)
    #                     #nans = np.isnan(recibido)
    #                     #recibido = np.where(nans, 0, recibido)
    #                     npt.assert_almost_equal(dts, recibido, 5)
    #                     print("Done")
    #                     # equal to 5 decimal places

    def test_incrementar(símismo):
        n_pasos = 5
        for n in range(len(testDirectories)):
            with símismo.subTest(datos=testDirectories[n]), IDMEnchufes() as servidor:
                símismo._empezar_cliente(servidor.dirección, servidor.puerto, n)
                servidor.activar()
                servidor.incrementar(n_pasos)
                t = servidor.recibir('t')
                print("T is: ", t)
                símismo.assertEqual(t, n_pasos)

    def test_finalizar(símismo):
        for n in range(len(testDirectories)):
            with símismo.subTest(datos=testDirectories[n]), IDMEnchufes() as servidor:
                símismo._empezar_cliente(servidor.dirección, servidor.puerto, n)
                servidor.activar()
                servidor.finalizar()
                t = servidor.recibir('t')
                print("T is: ", t)
                símismo.assertEqual(t, t_final[n])

    def tearDown(símismo):
        for cliente in símismo.clientes:
            cliente.wait()
