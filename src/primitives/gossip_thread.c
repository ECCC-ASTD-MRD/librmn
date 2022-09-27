/* RMNLIB - Library of useful routines for C and FORTRAN programming
 * Copyright (C) 1975-2000  Division de Recherche en Prevision Numerique
 *                          Environnement Canada
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, write to the
 * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 */

#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>
#include <string.h>
#include <strings.h>
#include <unistd.h>
#include <sys/types.h>
#include <fcntl.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>
#include <time.h>
#include <rmn/rpnmacros.h>

#include <rmn/gossip.h>


#define MAX_LOCKS 256


static pthread_mutex_t mutex = PTHREAD_MUTEX_INITIALIZER;
static pthread_mutex_t locks[MAX_LOCKS] ;
static int locks_initialized = 0;
//! Number of active clients
static int client_no = 0;
//! Total number of clients served
static int client_ord = 0;
//! Flag to indicate pending SHUTDOWN request
static int exit_requested = 0;
static extendedClientSlot clients[MAX_EXTENDED_CLIENTS];
static int client_table_initialized = 0;


void set_exit_requested() {
  exit_requested = 1;
}


int is_exit_requested() {
  return(exit_requested);
}


//! Initialize client table by setting everything to 0
static void initialize_client_table()  {
    if (client_table_initialized) return;

    memset(&clients[0], 0, sizeof(clients));
    client_table_initialized = 1;
}

//! Find a free slot in the client table
//! \return Slot number or -1 if there are no slots available
static int find_client_slot() {
    int slot = 0;
    while (clients[slot].client_id != 0 && slot < MAX_EXTENDED_CLIENTS) {
        slot++;
    }
    return slot < MAX_EXTENDED_CLIENTS ? slot : -1 ;
}


//! Initialize locks table from already initialized mutex
static void initialize_locks() {
    if (locks_initialized)  return;
    for (int i = 1 ; i < MAX_LOCKS ; i++) {
        memcpy(&locks[i], &mutex, sizeof(mutex));
    }
    locks_initialized = 1;
}


//! Exit from a client serving thread, reinitialize client structure
void exit_from_client_thread(extendedClientSlot *client) {
    if (client->command) free(client->command);
    memset(client, 0, sizeof(extendedClientSlot));
    pthread_exit(0);
}

//! Start a client module, non threaded
void start_client_module_2(
    void (*client_address)(),
    int client_uid,
    int client_pid,
    int fclient,
    char *command,
    void *data
) {
    extendedClientSlot client;
    client.uid = client_uid;
    client.pid = client_pid;
    client.socket = fclient;
    client.client_id = -1;
    client.command = command;
    client.data = NULL;
    client.user_function = NULL;
    (*client_address)(&client);
}


void start_client_thread_2(
    void (*client_address)(extendedClientSlot *),
    int client_uid,
    int client_pid,
    int fclient,
    char *command,
    void *data,
    void (*user_server)()
) {
    pthread_t client_thread[1];
    pthread_attr_t client_attr;

    initialize_locks();
    initialize_client_table();

    pthread_mutex_lock(&mutex);
    client_no++;
    client_ord++;
    pthread_mutex_unlock(&mutex);


    int slot = find_client_slot();
    clients[slot].uid = client_uid;
    clients[slot].pid = client_pid;
    clients[slot].socket = fclient;

    clients[slot].client_id = client_ord;
    clients[slot].command = (char *)malloc(strlen(command) + 1);
    strncpy(clients[slot].command, command, strlen(command) + 1);

    clients[slot].data = data;

    clients[slot].user_function = user_server;

    pthread_attr_init(&client_attr);
    pthread_create(&client_thread[0], &client_attr, (void *(*)(void *))client_address, (void *) &clients[slot]);

    pthread_detach(client_thread[0]);
}


//! Increment the count of connected clients (thread safe)
void increment_client_count() {
    pthread_mutex_lock(&mutex);
    client_no++ ;
    pthread_mutex_unlock(&mutex);
}

//! Decrement the count of connected clients (thread safe)
void decrement_client_count() {
    pthread_mutex_lock(&mutex);
    client_no-- ;
    client_ord--;
    pthread_mutex_unlock(&mutex);
}

//! Get the count of connected clients
int get_client_count() {
    return client_no > MAX_EXTENDED_CLIENTS ? -1 : client_no;
}


int exit_from_client(int fclient) {
    for (int i = 0; i < get_client_count(); i++) {
        if (clients[i].socket == fclient) {
            exit_from_client_thread((void *)&clients[i]);
            return 0;
        }
    }

    return -1;
}
