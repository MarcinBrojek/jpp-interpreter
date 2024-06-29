void writeIfFalse(bool ok) { if (!ok) { cout << "not ok\n"; } }
void writeIfTrue(bool ok) { if (ok) { cout << "not ok\n"; } }

int main() {
    writeIfFalse(1 < 2);
    writeIfTrue(1 > 2);
    writeIfFalse(3 <= 3);
    writeIfTrue(4 <= 3);
    writeIfFalse(3 == 3);
    writeIfTrue(4 == 3);
    writeIfFalse(3 >= 3);
    writeIfTrue(3 >= 4);
    writeIfFalse(2 > 1);
    writeIfTrue(1 > 2);
    writeIfFalse(0 != 1);
    writeIfTrue(1 != 1);

    writeIfFalse(true == true);
    writeIfTrue(false == true);
    writeIfFalse(true != false);
    writeIfTrue(false != false);

    writeIfFalse("abc" == "abc");
    writeIfTrue("abc" == "cba");

    return 0;
}