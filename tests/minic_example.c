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

struct firstStruct nonInitilized;

int PARAM = 5;

int **PTR;

int *PTR2 = &PARAM;

struct B b1 = { 5 };

int fact(int n) {
    if (n < 2) {
        return 1;
    } else {
        return n * fact(n-1);
    }
}

int addOne(int i) {
  int j = i + 1;
  return j;
}

struct firstStruct addBoth(struct firstStruct s, int i) {
  struct firstStruct tmp = {s.a + i, s.b + i};
  if(i > 0) {
    return tmp;
  } else {
    return {s.a + i, s.b + i};
  }
}

void main() {
    putchar(PARAM);
    if (5 <= 12) {
      putchar(5);
    } else {
      putchar(12);
    }

    int i = 0;
    struct firstStruct structExample = {2, (2+2)};

    while(i < 10) {
      putchar(i);
      i = i + 1;
    }

    struct myStruct example = {{1, 2}, 2};
    struct firstStruct *strPtr;
    structExample = {2, 2};
    structExample.a = 14;
    *strPtr = {1, 2};
    example.theStr.a = 42;

    int v = (*strPtr).a;


    **PTR = 5;

    struct A a = {PTR, *PTR};
    structExample = addBoth(structExample, 2);
    structExample.b = addBoth(structExample, -6).b;

    int w = addOne(12);

    //structExample.b = fact(5);
    // Attention ceci est permis mais
    // peut créer une erreur si l'adresse
    // finale n'est pas valide, ou modifier
    // une variable quelconque si elle est valide.
     **(PTR + 1) = 12;

    putchar(fact(PARAM));
}

