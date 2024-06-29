int main() {
    int functionId(int x) {
        return x * 2 / 2;
    }

    void procedureId(int x) {
        int a = 2 * x - x;
        if (a != x) {
            cout << "bad procedure id\n";
        }
    }

    int x = 42;
    if (functionId(x) != x) {
        cout << "bad function id\n";
    }
    procedureId(x);

    return 0;
}