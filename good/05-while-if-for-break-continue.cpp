void intEq(int a, int b) { if (a != b) { cout << a << " not eq " << b << "\n"; } }

int main() {
    int x = 0;
    for (int i = 0; i < 10; i++) {
        x++;
        if (i > 5) {
            continue;
        }
        x++;
    }
    intEq(x, 16);

    while (x > 0) {
        x--;
        if (x == 5) {
            break;
        }
    }
    intEq(x, 5);

    while (x < 10) {
        x++;
    }
    intEq(x, 10);

    return 0;
}