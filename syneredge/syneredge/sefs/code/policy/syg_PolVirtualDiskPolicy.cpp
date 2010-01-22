
#include <PolVirtualDiskPolicy.hpp>
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
    fprintf(stderr, "    PolVirtualDiskPolicyMain insert name policy\n");
    fprintf(stderr, "    PolVirtualDiskPolicyMain delete name\n");
    fprintf(stderr, "    PolVirtualDiskPolicyMain select\n");
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
 * Command line interface to PolVirtualDiskPolicy class.
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

    if (strcasecmp(argv[1], "insert") == 0) {
        checkNumArgs(argc, 4);
        PolVirtualDiskPolicy vdp;
        if (!vdp.Insert(argv[2], argv[3])) {
            fprintf(stderr, "%s\n", vdp.Error());
            exit(1);
        }
        ok = true;
    }

    if (strcasecmp(argv[1], "delete") == 0) {
        checkNumArgs(argc, 3);
        PolVirtualDiskPolicy vdp;
        if (!vdp.Delete(argv[2])) {
            fprintf(stderr, "%s\n", vdp.Error());
            exit(1);
        }
        ok = true;
    }

    if (strcasecmp(argv[1], "select") == 0) {
        checkNumArgs(argc, 2);
        PolVirtualDiskPolicy vdp;
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
