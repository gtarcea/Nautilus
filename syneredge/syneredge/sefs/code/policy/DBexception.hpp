
#include <string.h>

class DBexception
{
    char *text;

    public:

    DBexception(const char *message)
    {
        text = new char[strlen(message)+1];
        strcpy (text, message);
    }

    // ------------------------------------------------------------------------

    ~DBexception(void)
    {
        if (text != NULL) {
            delete [] text;
        }
    }

    // ------------------------------------------------------------------------

    const char *toString(void)
    {
        return text;
    }
};
