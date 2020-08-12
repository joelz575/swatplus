import os
from tinamit.tinamit3 import IDMEnchufes
from tinamit.conect import Conectado
from tinamit.ejemplos import obt_ejemplo
from tinamit.envolt.bf.swat_plus.envolt import ModeloSWATPlus
from tinamit.tiempo import Tiempo, EspecTiempo
#import matplotlib.pyplot as plt just another comment

ModeloSWATPlus.estab_conf("exe",str(os.path.dirname(os.path.realpath(__file__)))+"/build/bin/swatplus_exe")
mdinamicasocial = obt_ejemplo('sencillo/mds_bosques.mdl')
#biofisica = ModeloSWATPlus("C:\\Users\\Joel\\Documents\\Prof Adamowski\\Iximulew\\SWAT+\\New SWAT+\\LagoAtitlan\\Scenarios\\Default\\TxtInOut", connectar= True)
biofisica = ModeloSWATPlus(str(os.path.dirname(os.path.realpath(__file__)))+"/Trial Robit/Scenarios/Default/TxtInOut", connectar= True)

modelo_actual = Conectado(bf=biofisica, mds=mdinamicasocial)

#Vamos a conectar los variables necesarios
modelo_actual.conectar(var_mds='Lluvia', var_bf='Lluvia', mds_fuente=False)
modelo_actual.conectar(var_mds='Bosques', var_bf='Bosques', mds_fuente=True)
#Y simulamos
resultados_conexion = modelo_actual.simular(EspecTiempo(10, '1990-01-01'))#, tmñ_paso=2))


#Visualizar
#f, (eje1, eje2) = plt.subplots(1,2)
#eje1.plot(resultados_conexion['mds']['Bosques'].vals)
#eje1.set_title('Bosques')
#eje1.set_xlabel('Meses')

#eje2.plot(resultados_conexion['mds']['Lluvia'].vals)
#eje2.set_title('Lluvia')
#eje2.set_xlabel('Meses')
#eje1.ticklabel_format(axis='y', style='sci', scilimits=(0,0))
#
#from tinamit.envolt.mds import gen_mds
#from tinamit.envolt.bf import gen_bf
#res_mds = gen_mds(mdinamicasocial).simular(200, nombre='Corrida_MDS')
#res_bf = gen_bf(biofisica).simular(200, nombre='Corrida_BF')
#
#
#
#
#
#Visulaizar
#f, (eje1, eje2) = plt.subplots(1,2)
#eje1.plot(resultados_conexion['mds']['Bosques'].vals, label='Conectado')
#eje1.plot(res_mds['Bosques'].vals, label='Individual')
#eje1.set_title('Bosques')
#eje1.set_xlabel('Meses')
#
#eje1.ticklabel_format(axis='y', style='sci', scilimits=(0,0))
#
#eje2.plot(resultados_conexion['mds']['Lluvia'].vals)
#eje2.plot(res_bf['Lluvia'].vals)
#eje2.set_title('Lluvia')
#eje2.set_xlabel('Meses')

#f.legend()
#plt.show()