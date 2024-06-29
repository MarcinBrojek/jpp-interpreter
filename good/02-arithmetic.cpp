void intEq(int a, int b) { if (a != b) { cout << a << " not eq " << b << "\n"; } }
void boolEq(bool a, bool b) { if (a != b) { cout << a << " not eq " << b << "\n"; } }
void stringEq(string a, string b) { if (a != b) { cout << a << " not eq " << b << "\n"; } }

int main() {
    intEq(2 + 2, 4);
    intEq(5 - 3, 2);
    intEq(2 * 3, 6);
    intEq(10 / 5, 2);
    intEq(7 % 3, 1);

    boolEq(true && false, false);
    boolEq(true || false, true);
    boolEq(!true, false);

    stringEq("ab" + "cd", "abcd");

    return 0;
}