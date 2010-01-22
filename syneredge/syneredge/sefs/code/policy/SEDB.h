
#ifndef SEDB_H
#define SEDB_H

// Definitions for each table and field in the sedb database.
// Generated: Sat Jun  4 13:45:50 EDT 2005 by irrer
// This file was automatically generated with gen_header.


typedef struct SEDB_block_file_Struct {
    char host[25];
    char directory[31];
    int64 max_bytes;
    int64 bytes;
    char room[13];
    char building[13];
} SEDB_block_file;
#define BLOCK_FILE__NUM_FIELD (6)

typedef struct SEDB_file_system_Struct {
    char name[25];
    char virtual_disk[25];
} SEDB_file_system;
#define FILE_SYSTEM__NUM_FIELD (2)

typedef struct SEDB_virtual_disk_Struct {
    char name[25];
    char host[25];
    char directory[31];
    int64 bytes;
    char virtual_disk_policy[25];
    int32 block_size;
} SEDB_virtual_disk;
#define VIRTUAL_DISK__NUM_FIELD (6)

typedef struct SEDB_virtual_disk_policy_Struct {
    char name[25];
    char *policy;
} SEDB_virtual_disk_policy;
#define VIRTUAL_DISK_POLICY__NUM_FIELD (2)
// Table block_file 
typedef char SEDB_block_file__host[25];
typedef char SEDB_block_file__directory[31];
typedef int64 SEDB_block_file__max_bytes;
typedef int64 SEDB_block_file__bytes;
typedef char SEDB_block_file__room[13];
typedef char SEDB_block_file__building[13];

// Table file_system 
typedef char SEDB_file_system__name[25];
typedef char SEDB_file_system__virtual_disk[25];

// Table virtual_disk 
typedef char SEDB_virtual_disk__name[25];
typedef char SEDB_virtual_disk__host[25];
typedef char SEDB_virtual_disk__directory[31];
typedef int64 SEDB_virtual_disk__bytes;
typedef char SEDB_virtual_disk__virtual_disk_policy[25];
typedef int32 SEDB_virtual_disk__block_size;

// Table virtual_disk_policy 
typedef char SEDB_virtual_disk_policy__name[25];
typedef char *SEDB_virtual_disk_policy__policy;

#endif
