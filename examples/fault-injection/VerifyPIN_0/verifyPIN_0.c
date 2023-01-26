#include <stdbool.h>
#include <assert.h>

typedef signed char   SBYTE;
typedef unsigned char UBYTE;
typedef unsigned char BOOL;
typedef unsigned long ULONG;

#define BOOL_TRUE 0xAA
#define BOOL_FALSE 0x55
#define INITIAL_VALUE 0x2a
#define PIN_SIZE 4

BOOL g_authenticated;
SBYTE g_ptc;
UBYTE g_countermeasure;
UBYTE g_userPin[PIN_SIZE];
UBYTE g_cardPin[PIN_SIZE];

void initialize()
{
   // local variables
   int i;
   // global variables initialization
   g_authenticated = 0;
   g_ptc = 3;
   g_countermeasure = 0;
   // card PIN = 1 2 3 4 5...
   for (i = 0; i < PIN_SIZE; ++i) {
       g_cardPin[i] = i+1;
   }
   // user PIN = 0 0 0 0 0...
   for (i = 0 ; i < PIN_SIZE; ++i) {
       g_userPin[i] = 0;
   }
}

BOOL byteArrayCompare(UBYTE* a1, UBYTE* a2, UBYTE size)
{
    int i;
    for(i = 0; i < size; i++) {
        if(a1[i] != a2[i]) {
            return 0;
        }
    }
    return 1;
}


BOOL verifyPIN() {
    g_authenticated = 0;

    if(g_ptc > 0) {
        if(byteArrayCompare(g_userPin, g_cardPin, PIN_SIZE) == 1) {
            g_ptc = 3;
            g_authenticated = 1; // Authentication();
            return 1;
        } else {
            g_ptc--;
            return 0;
        }
    }

    return 0;
}

BOOL oracle()
{
    return g_countermeasure != 1 && g_authenticated == 1;
}


int main()
{
    initialize();
    verifyPIN();
    assert(oracle());

    return 0;
}
