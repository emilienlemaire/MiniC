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
    putchar(fact(PARAM));
}

