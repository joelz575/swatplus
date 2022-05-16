import os
from subprocess import Popen
from unittest import TestCase
import numpy.testing as npt
from tinamit_idm.puertos import IDMEnchufes

from swatplus_test.example_client import model_vars, sample_data

t_final = [3287, 3287, 731, 4018]
valgrind = False
BASE_DIR = os.path.split(os.path.split(__file__)[0])[0]
testDirectories = ["ceap_connectivity test",
                   "saturated_buffer",
                   "Texas_large_gully",
                   "Usa_Basin_model"]

class TestIDM(TestCase):

    def setUp(self):
        self.clients = []

    def start_client(self, host, port, n):
        cwd = os.path.join(BASE_DIR, "swatplus_test/" + testDirectories[n])
        swat_exe = os.path.join(BASE_DIR, "build/bin/swatplus_exe")

        if valgrind:
            client = Popen(["valgrind", "--leak-check=yes", "--track-origins=yes", swat_exe, str(port), host],
                            cwd=cwd)
        else:
            client = Popen([swat_exe, str(port), host], cwd=cwd)

        self.clients.append(client)
        return client

    def test_opening_closing(self):
         for n in range(len(testDirectories)):
               with self.subTest(sample_data=testDirectories[n]), IDMEnchufes() as server:
                   self.start_client(server.dirección, server.puerto,n)
                   server.activar()
                   server.cerrar()

    def test_set_and_send(self):
        for n in range(len(testDirectories)):
            if model_vars[testDirectories[n]] != "":
                for var_name in model_vars.get(testDirectories[n]):
                    dts = sample_data[var_name]
                    with self.subTest(sample_data=testDirectories[n]+"/"+var_name), IDMEnchufes() as server:
                        self.start_client(server.dirección, server.puerto, n)
                        server.activar()
                        server.cambiar(var_name, dts) if sample_data[var_name].dtype is int else server.cambiar(var_name, dts, precisión=5)
                        reply = server.recibir(var_name) if sample_data[var_name].dtype is int else server.recibir(var_name, precisión=5)
                        npt.assert_almost_equal(dts, reply, 5)
                        # equal to 5 decimal places

    def test_advancing_model(self):
         n_pasos = 5
         for n in range(len(testDirectories)):
             with self.subTest(sample_data=testDirectories[n]), IDMEnchufes() as server:
                 self.start_client(server.dirección, server.puerto, n)
                 server.activar()
                 server.incrementar(n_pasos)
                 t = server.recibir('t')
                 self.assertEqual(t, n_pasos)
    
    def test_finalizing_model(self):
        for n in range(len(testDirectories)):
            with self.subTest(sample_data=testDirectories[n]), IDMEnchufes() as server:
                self.start_client(server.dirección, server.puerto, n)
                server.activar()
                server.finalizar()
                t = server.recibir('t')
                self.assertEqual(t, t_final[n])

    def tearDown(self):
        for client in self.clients:
            client.wait()
