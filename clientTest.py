import os
from subprocess import Popen
from unittest import TestCase

import numpy.testing as npt

from tinamit.envolt.bf.swat_plus.enchufe import ModeloEnchufe
from tinamit.envolt.bf.swat_plus._vars import VariableEnchufe, obt_test_vars
from tinamit.mod import VariablesMod

t_final = 15
variables = []
dic_ingr = obt_test_vars()
for nmbr, info in dic_ingr.items():
    variables.append(
                VariableEnchufe(nombre=nmbr, código=info["código"], unid=info["unid"], inic=info["val"], ingr=info["ingr"], egr=info["egr"]))
    testVariables = VariablesMod(variables)

class Prueba(TestCase):

    def setUp(símismo):
        símismo.clientes = []

    def _empezar_cliente(símismo, dirección, puerto):
        cliente = Popen([os.getcwd()+"/build/bin/swatplus_exe", str(puerto), dirección],
                        cwd="Trial Robit/Scenarios/Default/TxtInOut")
        símismo.clientes.append(cliente)
        return cliente

    def test_abrir_cerrar(símismo):
        with ModeloEnchufe() as servidor:
            símismo._empezar_cliente(servidor.dirección, servidor.puerto)
            servidor.activar()
            servidor.cerrar()

    def test_mandar_datos(símismo):
        for nmbr_dts, dts in dic_ingr.items():
            with símismo.subTest(datos=nmbr_dts), ModeloEnchufe() as servidor:
                símismo._empezar_cliente(servidor.dirección, servidor.puerto)
                servidor.activar()
                servidor.cambiar_var(dts)

                recibido = servidor.recibir('var')
                npt.assert_equal(dts, recibido)

    def test_recibir_datos(símismo):
        for nmbr_dts, dts in dic_ingr.items():
            with símismo.subTest(datos=nmbr_dts), ModeloEnchufe() as servidor:
                símismo._empezar_cliente(servidor.dirección, servidor.puerto)
                servidor.activar()
                recibido = servidor.recibir(nmbr_dts)
                npt.assert_equal(dts, recibido)

    def test_incrementar(símismo):
        n_pasos = 5
        with ModeloEnchufe() as servidor:
            símismo._empezar_cliente(servidor.dirección, servidor.puerto)
            servidor.activar()
            servidor.incrementar(n_pasos)

            t = servidor.recibir('t')
            símismo.assertEqual(t, n_pasos)

    def test_finalizar(símismo):
        with ModeloEnchufe() as servidor:
            símismo._empezar_cliente(servidor.dirección, servidor.puerto)
            servidor.activar()
            servidor.finalizar()

            t = servidor.recibir('t')
            símismo.assertEqual(t, t_final)

    def tearDown(símismo):
        for cliente in símismo.clientes:
            cliente.wait()