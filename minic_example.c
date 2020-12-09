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

int fact(int n) {
    if (n < 2) {
        return 1;
    } else {
        return n * fact(n+-1);
    }
}

void main() {
    if (5 <= 12) {
      putchar(5);
    } else {
      putchar(12);
    }

    struct firstStruct structExample = {2, (2+2)};
    struct myStruct example = {{1, 2}, 2};
    struct firstStruct *strPtr;
    structExample = {2, 2};
    structExample.a = 14;
    *strPtr = {1, 2};
    example.theStr.a = 42;

    **PTR = 5;

    struct A a = {PTR, *PTR};

    putchar(example.theStr.a);
    putchar(fact(PARAM));
}

