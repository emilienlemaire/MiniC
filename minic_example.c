struct str {
  int a;
  int b;
};

int PARAM = 5;

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
    str structExample = {2, (2+2)};
    structExample = {2, 2};
    structExample.a = 14;
    putchar(structExample.a);
    putchar(fact(PARAM));
}

