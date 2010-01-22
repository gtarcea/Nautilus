
#include <PolBlockFile.hpp>
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
    fprintf(stderr, "    PolBlockFileMain insert host directory bytes room building\n");
    fprintf(stderr, "    PolBlockFileMain delete host directory\n");
    fprintf(stderr, "    PolBlockFileMain select\n");
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
 * Command line interface to PolBlockFile class.
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
#if BLOCK_FILE__NUM_FIELD != 6
    Force compilation error : Wrong number of database fields.
#endif

    if (strcasecmp(argv[1], "insert") == 0) {
        checkNumArgs(argc, 7);
        PolBlockFile vdp;
        const char *fields[BLOCK_FILE__NUM_FIELD+1];
        fields[0] = argv[2];  // host
        fields[1] = argv[3];  // directory
        fields[2] = argv[4];  // max_bytes
        fields[3] = argv[4];  // bytes
        fields[4] = argv[5];  // room
        fields[5] = argv[6];  // building
        fields[6] = NULL;    // terminator
        if (!vdp.Insert(fields)) {
            fprintf(stderr, "%s\n", vdp.Error());
            exit(1);
        }
        ok = true;
    }

    if (strcasecmp(argv[1], "delete") == 0) {
        checkNumArgs(argc, 4);
        PolBlockFile vdp;
        if (!vdp.Delete(argv[2], argv[3])) {
            fprintf(stderr, "%s\n", vdp.Error());
            exit(1);
        }
        ok = true;
    }

    if (strcasecmp(argv[1], "select") == 0) {
        checkNumArgs(argc, 2);
        PolBlockFile vdp;
        if (!vdp.Select()) {
            fprintf(stderr, "%s\n", vdp.Error());
            exit(1);
        }
        ok = true;

        // print out the table
        int row = 0;
        int col = 0;
        const char *text;
        
        while (((text = vdp.get(row, col)) != NULL) || (col != 0)) {
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
