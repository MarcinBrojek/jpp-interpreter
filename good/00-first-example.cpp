bool is_ok(int value) {
    if (value == 42) {
        return true;
    }
    return false;
}

int main() {
    int value = 0;
    while (!is_ok(value)) {
        value++;
    }
    cout << "szukana liczba to " << value << "\n";
  
    list<int> history;
    int expexted_sum_without_0 = (1 + value) * 42 / 2;
    int expected_sum_with_0 = 0;
  
    for (int i = value; i >= 0; i--) {
        history -> push_front(i);
        expected_sum_with_0 += i;
    }
  
    void check_sums(int s1, int s2, bool &same, string &msg) {
        same = (s1 == s2);
        if (same == true) {
            msg = "sumy takie same";
        } else {
            msg = "sumy nie takie same";
        }
    }
  
    bool is_ok; string message;
    check_sums(expexted_sum_without_0, expected_sum_with_0, is_ok, message);
    tuple<bool, string, list<int>> info = make_tuple(is_ok, message, history);

    // example extend
    cout << info << "\n";

    return 0;
}
