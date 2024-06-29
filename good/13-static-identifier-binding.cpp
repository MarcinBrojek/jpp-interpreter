int f() {
    return 0;
}

int main() {
    if (f() != 0) {
        cout << "should return 0\n";
    }

    int x = 1;

    int f() {
        return x;
    }
    if (f() != 1) {
        cout << "should return 1\n";
    }

    x = 2;

    int g() {
        int x = 3;
        return f();
    }
    if (g() != 2) {
        cout << "should return 2\n";
    }

    return 0;
}