void intEq(int a, int b) { if (a != b) { cout << a << " not eq " << b << "\n"; } }
void boolEq(bool a, bool b) { if (a != b) { cout << a << " not eq " << b << "\n"; } }
void stringEq(string a, string b) { if (a != b) { cout << a << " not eq " << b << "\n"; } }

int main() {
    int x = 1, y = 2;
    x++;
    intEq(x, 2);
    x += 2;
    intEq(x, 4);
    x--;
    intEq(x, 3);
    x -= (-12);
    intEq(x, 15);
    x *= 2;
    intEq(x, 30);
    x /= 3;
    intEq(x, 10);
    x %= 5;
    intEq(x, 0);
    x = y;
    intEq(x, 2);

    bool a = true, b = false;
    a &&= false;
    boolEq(a, false);
    a ||= true;
    boolEq(a, true);
    a = b;
    boolEq(a, false);

    string s = "abc", t = "def";
    s += t;
    stringEq(s, "abcdef");
    s = t;
    stringEq(s, "def");

    return 0;
}