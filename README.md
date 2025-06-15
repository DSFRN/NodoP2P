# Nodo - Red LAN Distribuida Peer-to-Peer
Trabajo Práctico Final - SO I
- Franco Di Santis
- Juan Ignacio Cicerchia

## Instrucciones:

Corre el comando `make` para iniciar el servidor del nodo.

Para iniciar un proceso *cliente* puedes correr el siguiente comando dentro de la EShell:
`nodo:connect().`

Commandos disponibles como cliente:[^1]
- id_nodo - Retorna la ID del nodo servidor al cuál está conectado.
- listar_archivos - Lista los archivos que el nodo servidor tiene para compartir.
- salir - Cierra la conexión con el nodo servidor.

> [!TIP]
> Corre el comando `make clean` al finalizar la ejecución para limpiar los archivos generados automáticamente.

[^1]: Cualquier otro comando, si bien no será aceptado, no cerrará la conexión.
