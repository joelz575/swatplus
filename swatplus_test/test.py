import os
import tracemalloc
from subprocess import Popen
from unittest import TestCase

import numpy.testing as npt
from tinamit.idm.puertos import IDMEnchufes

from swatplus_test.ejemplo_cliente import leer_datos, pequeno_datos

t_final = 731
valgrind = True
BASE_DIR = os.path.split(os.path.split(__file__)[0])[0]


class PruebaIDM(TestCase):

    def setUp(símismo):
        símismo.clientes = []

    def _empezar_cliente(símismo, dirección, puerto):
        cwd = os.path.join(BASE_DIR, "Texas_large_gully")
        swat_exe = os.path.join(BASE_DIR, "build/bin/swatplus_exe")
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

    def test_mandar_datos(símismo):
        for nmbr_dts, dts in pequeno_datos.items():
            with símismo.subTest(datos=nmbr_dts), IDMEnchufes() as servidor:
                símismo._empezar_cliente(servidor.dirección, servidor.puerto)
                servidor.activar()
                print("going to send this data: ", dts)
                servidor.cambiar(nmbr_dts, dts)
                servidor.cambiar(nmbr_dts, dts)
                recibido = servidor.recibir(nmbr_dts)
                # npt.assert_almost_equal(dts, recibido)
                # equal to 7 decimal places
                npt.assert_almost_equal(dts, recibido, 5)
                # equal to 5 decimal places

    def test_ciclo_completo(símismo):
        npasos = 5
        t = 0
        #tracemalloc.start()
        for nmbr_dts, dts in pequeno_datos.items():
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
                    #snapshot = tracemalloc.take_snapshot()
                    #top_stats = snapshot.statistics('lineno')
                    #print("[ Top 10 ]")
                    #for stat in top_stats[:10]:
                    #    print(stat)
                    t = servidor.recibir('t')
                    check_t = check_t + npasos
                    npt.assert_equal(check_t, t)

    def test_recibir_datos(símismo):
        for nmbr_dts, dts in leer_datos.items():
            with símismo.subTest(datos=nmbr_dts), IDMEnchufes() as servidor:
                símismo._empezar_cliente(servidor.dirección, servidor.puerto)
                servidor.activar()
                recibido = servidor.recibir(nmbr_dts)
                print("receiving ", nmbr_dts)
                print("received ", recibido)
                # npt.assert_almost_equal(dts, recibido)
                # equal to 7 decimal places
                npt.assert_almost_equal(dts, recibido, 5)
                # equal to 5 decimal places

    def test_incrementar(símismo):
        n_pasos = 5
        with IDMEnchufes() as servidor:
            símismo._empezar_cliente(servidor.dirección, servidor.puerto)
            servidor.activar()
            servidor.incrementar(n_pasos)
            t = servidor.recibir('t')
            print("T is: ", t)
            símismo.assertEqual(t, n_pasos)

    def test_finalizar(símismo):
        with IDMEnchufes() as servidor:
            símismo._empezar_cliente(servidor.dirección, servidor.puerto)
            servidor.activar()
            servidor.finalizar()
            t = servidor.recibir('t')
            print("T is: ", t)
            símismo.assertEqual(t, t_final)

    def tearDown(símismo):
        for cliente in símismo.clientes:
            cliente.wait()
