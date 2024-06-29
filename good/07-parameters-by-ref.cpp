void inc(int& x) {
    x++;
}

int main() {
    int x = 0;
    inc(x);
    if (x != 1) {
        cout << "inc not effect\n";
    }

    return 0;
}