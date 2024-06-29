// IMPORTANT: method get not working in type checker
// for now it always returns type of first element

int main() {
    tuple<int, bool> t = make_tuple(42, false);

    if (t != make_tuple(42, false)) {
        cout << "tuples are the same\n";
    }
    if (t == make_tuple(42, true)) {
        cout << "tuples are different\n";
    }

    int x; bool b;
    tie(x, b) = t;
    if (x != 42) {
        cout << "first element should be 42\n";
    }
    if (b != false) {
        cout << "second element should be false\n";
    }

    tuple<tuple<string, bool>, int> pairAndNumber = make_tuple(make_tuple("ok", true), 1);
    tuple<string, bool> pair = make_tuple("ok", true);
    tie(pair, x) = pairAndNumber;
    if (x != 1) {
        cout << "x should be 1\n";
    }

    return 0;
}