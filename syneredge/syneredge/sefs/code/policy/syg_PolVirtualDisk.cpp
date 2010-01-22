
#include <PolVirtualDisk.hpp>
#include <stdio.h>
#include <stdlib.h>

// ------------------------------------------------------------------------

/**
 * Tell the user how to use this program.
 *
 * @param msg Message to print.
 */

static void usage(const char *msg)
{
    fprintf(stderr, "Usage: \n");
    fprintf(stderr, "    PolVirtualDiskMain insert name policy bytes block_size\n");
    fprintf(stderr, "    PolVirtualDiskMain delete name\n");
    fprintf(stderr, "    PolVirtualDiskMain select\n");
    fprintf(stderr, "\n");
    fprintf(stderr, "Error: %s\n", msg);
    exit(1);
}

// ------------------------------------------------------------------------

/**
 * Check that the number of arguments is correct.
 *
 * @param given Number of arguments given.
 *
 * @param req Number of arguments required.
 *
 */
static void checkNumArgs(int given, int req)
{
    if (given != req) {
        usage("Wrong number of arguments");
    }
}

// ------------------------------------------------------------------------

/**
 * Command line interface to PolVirtualDisk class.
 *
 * @param argc Number of command line arguments.
 *
 * @param argv List of command line arguments.
 */
int main(int argc, char **argv)
{
    bool ok = false;
    if (argc < 2) {
        usage("No arguments given");
    }

// force compile error on wrong number of fields
#if VIRTUAL_DISK__NUM_FIELD != 6
    Wrong number of database fields.
#endif

    if (strcasecmp(argv[1], "insert") == 0) {
        checkNumArgs(argc, 6);
        PolVirtualDisk virtDisk;
        if (!virtDisk.Insert(argv[2], argv[3], argv[4], argv[5])) {
            fprintf(stderr, "%s\n", virtDisk.Error());
            exit(1);
        }
        ok = true;
    }

    if (strcasecmp(argv[1], "delete") == 0) {
        checkNumArgs(argc, 3);
        PolVirtualDisk virtDisk;
        if (!virtDisk.Delete(argv[2])) {
            fprintf(stderr, "%s\n", virtDisk.Error());
            exit(1);
        }
        ok = true;
    }

    if (strcasecmp(argv[1], "select") == 0) {
        checkNumArgs(argc, 2);
        PolVirtualDisk virtDisk;
        if (!virtDisk.Select()) {
            fprintf(stderr, "%s\n", virtDisk.Error());
            exit(1);
        }
        ok = true;

        // print out the table
        int row = 0;
        int col = 0;
        const char *text;
        
        while (((text = virtDisk.get(row, col)) != NULL) || (col != 0)) {
            if (text == NULL) {
                col = 0;
                row++;
                printf("\n");
            }
            else {
                printf("%s   ", text);
                col++;
            }
        }
    }
    if (!ok) {
        usage("Unknown command");
    }
    exit (0);
}
