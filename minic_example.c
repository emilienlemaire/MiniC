struct firstStruct {
  int a;
  int b;
};

struct myStruct {
    struct firstStruct theStr;
    int b;
};

struct A {
  int **a;
  int *b;
};

struct B {
  int a;
};

int PARAM = 5;

int **PTR;

struct B b1 = { 5 };

int fact(int n) {
    if (n < 2) {
        return 1;
    } else {
        return n * fact(n-1);
    }
}

struct firstStruct addBoth(struct firstStruct s, int i) {
    return {s.a + i, s.a + i};
}

void main() {
    if (5 <= 12) {
      putchar(5);
    } else {
      putchar(12);
    }

    int i = 0;

    while(i < 10) {
      putchar(i);
    }

    struct firstStruct structExample = {2, (2+2)};
    struct myStruct example = {{1, 2}, 2};
    struct firstStruct *strPtr;
    structExample = {2, 2};
    structExample.a = 14;
    *strPtr = {1, 2};
    example.theStr.a = 42;

    int v = (*strPtr).a;

    **PTR = 5;

    struct A a = {PTR, *PTR};

    structExample.a = addBoth(structExample, 2).a;

    structExample.b = fact(5);
     **(PTR + 1) = 12;

    putchar(example.theStr.a);
    putchar(strPtr->a);
    putchar(fact(PARAM));
}

