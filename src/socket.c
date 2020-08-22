#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <unistd.h>
#include <string.h>
#include <json.h>
#include <json_tokener.h>
#ifdef _WIN32
	#include <WinSock2.h> // windows
#else
	#include <sys/socket.h>  // unicode
	#include <arpa/inet.h>
#endif




void sendr_(int *client, char *senderBuffer);
void jsonSTRtoFLTarray(float floatCont[], int *arraySize, char *jsonString);
void jsonSTRtoINTarray(int intCont[], int *arraySize, char *jsonString);

void opensocket_(int *portNum, char *hostNum, int *client){
#ifdef _WIN32
 printf("C function: Opening socket");
 printf("\nPortNum in C: %i",*portNum);

 int portNumber = *portNum;
 WSADATA winsockData;
 if (WSAStartup(MAKEWORD(2,2), &winsockData) != 0) {
            fprintf(stderr, "WSAStartup failed.\n");
            exit(1);
 }
 printf("\nWSAStartup was done successfully...");
 printf((char *)WSAGetLastError());
 SOCKET cliente;

 cliente = socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
 printf("client socket was created successfully...");
 printf((char *)WSAGetLastError());
 printf("\nClient obj in C: %d", cliente);
 struct sockaddr_in server_address;
 server_address.sin_family  = AF_INET;
 server_address.sin_addr.s_addr = inet_addr(hostNum);
 server_address.sin_port = htons(portNumber);
 printf((char *)WSAGetLastError());

 struct sockaddr_in TCPClientAdd;
 int iTCPClientAdd = sizeof(TCPClientAdd);


 int connectRes;
 connectRes = connect(cliente, (struct sockaddr *) &server_address, sizeof(server_address));
 printf((char *)WSAGetLastError());

 if (connectRes == -1){
	printf((char *)WSAGetLastError());
 	}
 	printf("\nConnect Result: %d", connectRes);
 	printf("\nCliente= %i", cliente);
 	*client = (int *)cliente;

#else
/* int sock = 0, valread;
    struct sockaddr_in serv_addr;
    char *hello = "Hello from client";
    char buffer[1024] = {0};
    if ((sock = socket(AF_INET, SOCK_STREAM, 0)) < 0)
    {
        printf("\n Socket creation error \n");
        return -1;
    }

    serv_addr.sin_family = AF_INET;
    serv_addr.sin_port = htons(portNum);

    // Convert IPv4 and IPv6 addresses from text to binary form
    if(inet_pton(AF_INET, hostNum, &serv_addr.sin_addr)<=0)
    {
        printf("\nInvalid address/ Address not supported \n");
        return -1;
    }

    if (connect(sock, (struct sockaddr *)&serv_addr, sizeof(serv_addr)) < 0)
    {
        printf("\nConnection Failed \n");
        return -1;
    }
*/
 printf("C function: Opening socket");
 printf("\nPortNum in C: %i",*portNum);
 int portNumber = *portNum;
 printf("\nHost Number in C: "); printf(hostNum);
 struct sockaddr_in serv_addr;

 if ((*client = socket(AF_INET, SOCK_STREAM, 0)) < 0)
    {
        printf("\n Socket creation error \n");
        //return -1;
    }
 else{
    printf("This is the client in C: %d\n", client);
 }
 serv_addr.sin_family = AF_INET;
 serv_addr.sin_port = htons(portNumber);
 serv_addr.sin_addr.s_addr = inet_addr(hostNum);
 /*if(inet_pton(AF_INET, hostNum, &serv_addr.sin_addr)<=0)
    {
        printf("\nInvalid address/ Address not supported \n");
        //return -1;
    }
 */
    printf("Connecting now\n");
    fflush(stdout);
 if (connect(*client, (struct sockaddr *)&serv_addr, sizeof(serv_addr)) < 0)
    {
        printf("\nConnection Failed \n");

    }

#endif
}

void receive_(int *client, char command[], char var_nombre[], char tip_con[], int *tamano_con, int intCont[], float floatCont[]){
 #ifdef _WIN32
 	//Cuando tenga una connexión exitosa, podemos recibir datos del socket.
 	//return
 	//setvbuf(stdout, NULL, _IONBF, 0);
 	printf("\n Am in receive now...");
 	fflush( stdout );
 	printf("\n INT buffer: %i", intCont);
 	printf("\n *tamano_con: %i", *tamano_con);
 	printf("\n FLOAT buffer: %f", floatCont);
 	fflush( stdout );
 	//printf("\n CHAR buffer: %s", charCont);
 	int recvRes;
 	int n;
	char blankBuffer;
 	char contBuffer[20000];
 	struct json_object *parsed_json;
 	struct json_object *orden;
 	struct json_object *tamano;
 	struct json_object *var;
 	struct json_object *matr;
 	struct json_object *contenido;
 	struct json_object *tipo_cont;
 	char json_header[20000];

	printf("Just before receive line...");
	fflush( stdout );
	n = recv(*client, &blankBuffer, 2, 0);
	printf("Size accoring to n variable: %d", n);
		fflush( stdout );
	printf("Size according to blank Buffer: %d", (int)blankBuffer);
		fflush( stdout );
	sendr_(client, "RCVD");
	printf((char *)WSAGetLastError());
		fflush( stdout );

 	printf("About to receive json_header....\n");
 		fflush( stdout );
 	n = recv(*client, json_header, ((int) blankBuffer), 0);
 	printf((char *)WSAGetLastError());
 		fflush( stdout );
 	printf("Receive Results from json_header: %i\n", n);
 		fflush( stdout );
 	printf("json_header: %s\n", json_header);
 		fflush( stdout );
 	//printf("Received json-header: %s\n", (char) json_header);
 	sendr_(client, "RCVD");

 	parsed_json = json_tokener_parse(json_header);
 	printf("Parsed json: %s\n", json_object_get_string(parsed_json));
 		fflush( stdout );
 	json_object_object_get_ex(parsed_json, "orden", &orden);
 	printf("Orden: %s\n", json_object_get_string(orden));
 		fflush( stdout );
 	json_object_object_get_ex(parsed_json, "tamaño", &tamano);
 	printf("%s\n", json_object_get_string(var));
 	json_object_object_get_ex(parsed_json, "var", &var);
 	printf("%s\n", json_object_get_string(var));
 		fflush( stdout );
 	json_object_object_get_ex(parsed_json, "matr", &matr);
 	printf("%s\n", json_object_get_string(matr));
 		fflush( stdout );
 	json_object_object_get_ex(parsed_json, "tipo_cont", &tipo_cont);
	printf("%s\n", json_object_get_string(tipo_cont));
	fflush(stdout);
	command = json_object_get_string(orden);
 	printf("Received Command: %s\n", command);
 		fflush( stdout );

 	if(strncmp(command, "TOMAR_", 6) == 0){
 		var_nombre = json_object_get_string(var);
 		printf("Received Variable: %s\n", var_nombre);
 			fflush( stdout );

 		tip_con = json_object_get_string(tipo_cont);

		printf("Receiving content buffer...");
			fflush( stdout );
 		recv(*client, contBuffer, 20000, 0);
		printf("Content Received: %s\n", contBuffer);
			fflush( stdout );
 		contenido = json_tokener_parse(contBuffer);

 		printf("This is the received matrix: %s\n", json_object_get_string(contenido));
 		fflush( stdout );
 		*tamano_con = (int *)json_object_array_length(contenido);
		printf("Array length of matrix: %d\n", *tamano_con);
		fflush( stdout );
 	 	if(*tamano_con > 0){
 	 		printf("Received matrix of length greater than 0...");
 			if(strncmp(tip_con, "int", 3) == 0){
 			tip_con = 'i';
 			printf("It is an int");
 			fflush(stdout);
			//intCont = (int *)malloc(sizeof(int)*((int) *tamano_con));
 	 		//charCont = (int *)malloc(sizeof(char)*0);
 	 		//floatCont = (int *)malloc(sizeof(double)*0);
 	 		memcpy(intCont, json_object_get_array(contenido), *tamano_con+1);
 	 		//*intCont = (int *) json_object_get_array(contenido);
 	 		printf("int buffer in C: %d\n", intCont[0]);
		    }
		    else if( strncmp(tip_con, "flt", 3) == 0){
			tip_con = 'f';
			printf("It is a float");
			fflush(stdout);
			floatCont = (int *)malloc(sizeof(double)*((int)*tamano_con));
 	 		//charCont = (int *)malloc(sizeof(char)*0);
 	 		intCont = (int *)malloc(sizeof(int)*0);
			*floatCont = (int *) json_object_get_array(contenido);
			printf("Json Object to array: %f", json_object_get_array(contenido)[0]);
			//for(i = 0; i < *tamano_con; i++){
			//	floatCont[i] = json_object_array_
			//}
			//memcpy(floatCont, json_object_get_array(contenido), *tamano_con+1);
			printf("float buffer in C: %f\n", floatCont[0]);
		    }
	    else{
		    printf("\n\n\n\n\nInvalid type for data transfer!!\n\n\n\n\n");
		    printf("Note: Transfering Data of string types are not yet supported.\n");
 	 	    //charCont = (int *)malloc(sizeof(char)*((int)*tamano_con));
 	 	    intCont = (int *)malloc(sizeof(int)*0);
 		    floatCont = (int *)malloc(sizeof(double)*0);
		}
	}
 		else{
 			//charCont = (int *)malloc(sizeof(char)*0);
 			printf("No data was transferred, in the exchange.");
 				fflush( stdout );
 			intCont = (int *)malloc(sizeof(int)*0);
 			floatCont = (int *)malloc(sizeof(double)*0);
		}

 	}

 	printf((char *)WSAGetLastError());
 		fflush( stdout );

	sendr_(client, "RCVD");
 #else
 	//Cuando tenga una connexión exitosa, podemos recibir datos del socket.
 	//return
 	//setvbuf(stdout, NULL, _IONBF, 0);
 	printf("\n Am in receive now...");
 	fflush( stdout );
 	printf("\n INT buffer: %i", intCont);
 	fflush( stdout );
 	printf("\n FLOAT buffer: %f", floatCont);
 	fflush( stdout );
 	//printf("\n CHAR buffer: %s", charCont);
 	int recvRes;
 	//printf("\nLength is: %i", *len);
 	int tmn;
 	int n;
 	int i;
 	char tipo_contenido;
	char blankBuffer;
 	char contBuffer[20000];
 	struct json_object *parsed_json;
 	struct json_object *orden;
 	struct json_object *tamano;
 	struct json_object *var;
 	struct json_object *matr;
 	struct json_object *contenido;
 	struct json_object *tipo_cont;
 	char json_header[2000];

	printf("Just before receive line...\n");
	fflush( stdout );

	n = read(*client, &blankBuffer, 4);
    printf("Size accoring to n variable: %d\n", n);
	fflush( stdout );
	printf("Size according to blank Buffer: %d\n", (int)blankBuffer);
	fflush( stdout );
	sendr_(client, "RCVD");
    printf("About to receive json_header....\n");
 	fflush( stdout );
 	n = read(*client, &json_header, (int) blankBuffer);
 	printf("Receive Results from json_header: %i\n", n);
 	sendr_(client, "RCVD");
 	fflush( stdout );
 	printf("Received json-header: %s\n", json_header);
 	fflush( stdout );
 	parsed_json = json_tokener_parse(json_header);
 	printf("Parsed json: %s", json_object_get_string(parsed_json));
 	fflush( stdout );
 	json_object_object_get_ex(parsed_json, "orden", &orden);
 	printf("Orden: %s", json_object_get_string(orden));
 	fflush( stdout );
 	json_object_object_get_ex(parsed_json, "tamaño", &tamano);
 	printf("Content size: %s", json_object_get_string(tamano));
 	json_object_object_get_ex(parsed_json, "var", &var);
 	printf("Var: %s", json_object_get_string(var));
 	fflush( stdout );
 	json_object_object_get_ex(parsed_json, "matr", &matr);
 	printf("MATR: %s", json_object_get_string(matr));
 	fflush( stdout );
 	printf("Just before tipo_cont...");
 	fflush( stdout );
 	json_object_object_get_ex(parsed_json, "tipo_cont", &tipo_cont);
	strncpy(command, json_object_get_string(orden),6);
 	printf("Received Command: %s\n", command);
 	fflush( stdout );

 	if(strncmp(command, "TOMAR_", 6) == 0){
 		strncpy(var_nombre, json_object_get_string(var), 6);
 		printf("Received Variable: %s\n", var_nombre);
 		printf("Tamano: %d\n", atoi((const char *) json_object_get_string(tamano)));
 		fflush( stdout );

 		strncpy(tip_con, json_object_get_string(tipo_cont), 3);

 		n = read(*client, &contBuffer, atoi((const char *) json_object_get_string(tamano)));

 		contenido = json_tokener_parse(contBuffer);
 		printf("This is the matrix: %s\n", json_object_get_string(contenido));
 		fflush( stdout );

 	    *tamano_con = (int *)json_object_array_length(contenido);
        printf("This is the length of the matrix: %d\n", *tamano_con);
 		fflush( stdout );

 	 	if(*tamano_con > 0){
 	 		printf("Received matrix of length greater than 0...");
 		if(strncmp(tip_con, "int", 3) == 0){
 			printf("It is an int");
 			fflush(stdout);
			intCont = (int *)malloc(sizeof(int)*((int) *tamano_con));
 	 		//charCont = (int *)malloc(sizeof(char)*0);
 	 		floatCont = (int *)malloc(sizeof(double)*0);
            jsonSTRtoINTarray(intCont, &tamano_con, json_object_get_string(contenido));
 	 		//memcpy(intCont, json_object_get_array(contenido), *tamano_con+1);
 	 		//*intCont = (int *) json_object_get_array(contenido);
 	 		printf("1st element of int buffer in C: %d\n", intCont[0]);
 	 		fflush(stdout);
		}
		else if( strncmp(tip_con, "flt", 3) == 0){
			printf("It is a float");
			fflush(stdout);
			floatCont = (int *)malloc(sizeof(double)*((int)*tamano_con));
 	 		//charCont = (int *)malloc(sizeof(char)*0);
 	 		intCont = (int *)malloc(sizeof(int)*0);
			//*floatCont = (int *) json_object_get_array(contenido);
			//printf("Json Object to array: %f", json_object_get_array(contenido)[0]);
			jsonSTRtoFLTarray(&floatCont, tamano_con, json_object_get_string(contenido));
			//memcpy(floatCont, json_object_get_array(contenido), *tamano_con+1);
			printf("float buffer in C: %f\n", floatCont[0]);
			fflush(stdout);
		}
		else{
			printf("\n\n\n\n\nInvalid type for data transfer!!\n\n\n\n\n");
			printf("Note: Transfering Data of string types are not yet supported.\n");
 	 		//charCont = (int *)malloc(sizeof(char)*((int)*tamano_con));
 	 		intCont = (int *)malloc(sizeof(int)*0);
 	 		floatCont = (int *)malloc(sizeof(double)*0);
		}
	}
 		else{
 			//charCont = (int *)malloc(sizeof(char)*0);
 			printf("No data was transferred, in the exchange.");
 				fflush( stdout );
 			intCont = (int *)malloc(sizeof(int)*0);
 			floatCont = (int *)malloc(sizeof(double)*0);
		}

 	}

	sendr_(client, "RCVD");
 #endif
	}

void jsonSTRtoFLTarray(float floatCont[], int *arraySize, char jsonString[]){
    int i; //counter
    int f=0; //counter
    int g=0; //counter
    int current=2; //counter helper
    char transferSTR[15] = "000000000000000";
    printf("\nInput variables are: %f, %d, %s", floatCont[0], (*arraySize), jsonString);
    fflush(stdout);
    printf("\nString length of jsonString: %d", strlen(jsonString));
    fflush(stdout);
    printf("\ni<*arraySize && f < strlen(jsonString): %c", i < *arraySize && f < strlen(jsonString));
    fflush(stdout);

    for(i=0; i < *arraySize && f < strlen(jsonString)-1; i++){
        f=current;
        printf("\nCurrently here in jsonString: %c", jsonString[f]);
        fflush(stdout);

        while(jsonString[f] != ' '){
            transferSTR[f-current] = jsonString[f];
            f++;
        }
        printf("\nTransfer String: %s", transferSTR);
        fflush(stdout);

        floatCont[i] = atof(transferSTR);
        printf("\nFloat Cont: %f", floatCont[i]);
        fflush(stdout);
        for(g=0; g<15; g++){
            transferSTR[g] = '0';
        }
        strncpy(transferSTR, "000000000000000", 15);
        printf("\nReset transferSTR: %s", transferSTR);
        fflush(stdout);
        current = f+1;
        printf("\nCurrent: %i", current);
        fflush(stdout);
    }
}

void jsonSTRtoINTarray(int intCont[], int *arraySize, char *jsonString){
    int i; //counter
    int f; //counter
    int g; //counter
    int current=2; //counter helper
    char transferSTR[15];

    for(i=0; i < *arraySize && f < strlen(jsonString); i++){
        f=current;
        while(strncmp((const char *)jsonString[f], " ", 1) != 0){
            transferSTR[f-current] = jsonString[f];
            f++;
        }
        printf("\nTransfer String: %s", transferSTR);
        fflush(stdout);
        intCont[i] = (int)atoll(transferSTR);
    }
}

void sendr_(int *client, char *senderBuffer){


 int iSenderBuffer;
 printf("Sending: %s", senderBuffer);
 fflush(stdout);
 iSenderBuffer = strlen(senderBuffer);
 int sendRes;
 //printf("Sending now....");
 sendRes = send(*client, senderBuffer, iSenderBuffer ,0);
 printf("Send Result: %d", sendRes);
 fflush(stdout);
 if (sendRes==-1){
	printf("send failed...");
#ifdef _WIN32
	printf((char *)WSAGetLastError());
#endif
		}

}

void closesock_(int *client){
#ifdef _WIN32
 int closeRes;
 closeRes = closesocket(*client);

 int iWSACleanup;	//print error if it didnt work
 iWSACleanup = WSACleanup();
#else
 shutdown(*client, 2);

#endif
}