##### Marcin Brojek

# Deklaracja języka do implementacji

---

##### **Gramatyka języka**

```haskell
entrypoints Program;
Program.            Program     ::= [Decl];

DeclFunc.           Decl        ::= Type Ident "(" [InitArg] ")" Body;
DeclProc.           Decl        ::= "void" Ident "(" [InitArg] ")" Body;
DeclVars.           Decl        ::= Type [InitVar] ";";
separator nonempty Decl "";

ArgWithValue.       InitArg     ::= Type Ident "=" Expr;
ArgRefWithValue.    InitArg     ::= Type "&" Ident "=" Expr;
ArgWithoutValue.    InitArg     ::= Type Ident;
ArgRefWithoutValue. InitArg     ::= Type "&" Ident;
separator InitArg ",";

VarWithValue.       InitVar     ::= Ident "=" Expr;
VarWithoutValue.    InitVar     ::= Ident;
separator nonempty InitVar ",";

Body.               Body        ::= "{" [Instr] "}";
separator nonempty Instr "";

LocalDeclFunc.      Instr       ::= Type Ident "(" [InitArg] ")" Body;
LocalDeclProc.      Instr       ::= "void" Ident "(" [InitArg] ")" Body;
LocalDeclVars.      Instr       ::= Type [InitVar] ";";
FuncReturn.         Instr       ::= "return" Expr ";";
ProcReturn.         Instr       ::= "return" ";";
Skip.               Instr       ::= ";";
ExecProc.           Instr       ::= Ident "(" [Expr] ")" ";";
While.              Instr       ::= "while" "(" Expr ")" Body;
If.                 Instr       ::= "if" "(" Expr ")" Body;
IfElse.             Instr       ::= "if" "(" Expr ")" Body "else" Body;
For.                Instr       ::= "for" "(" Type [InitVar] ";" Expr ";" Var Modif ")" Body;
Break.              Instr       ::= "break" ";";
Countinue.          Instr       ::= "continue" ";";
PushBack.           Instr       ::= Ident "->" "push_back" "(" Expr ")" ";";
PopBack.            Instr       ::= Ident "->" "pop_back" "(" ")" ";";
PushFront.          Instr       ::= Ident "->" "push_front" "(" Expr ")" ";";
PopFront.           Instr       ::= Ident "->" "pop_front" "(" ")" ";";

Cout.               Instr       ::= "cout" "<<" [CoutArgs] ";";
CoutArgs.           CoutArgs    ::= Expr;
separator nonempty CoutArgs "<<";

VarModif.           Instr       ::= Var Modif ";";
Iinc.               Modif       ::= "++";
IaddValue.          Modif       ::= "+=" Expr;
Idec.               Modif       ::= "--";
IdecValue.          Modif       ::= "-=" Expr;
IandEq.             Modif       ::= "&&=" Expr;
IorEq.              Modif       ::= "||=" Expr;
Imod.               Modif       ::= "%=" Expr;
Itimes.             Modif       ::= "*=" Expr;
Idiv.               Modif       ::= "/=" Expr;
Iassign.            Modif       ::= "=" Expr;

Var.                Var         ::= Ident;
VarStringEl.        Var         ::= Ident "[" Expr "]";
VarTie.             Var         ::= "tie" "(" [TieEl] ")";
TieEl.              TieEl       ::= Var;
separator nonempty TieEl ",";

Int.                Type        ::= "int";
Bool.               Type        ::= "bool";
String.             Type        ::= "string";
Tuple.              Type        ::= "tuple" "<" [Type] ">";
List.               Type        ::= "list" "<" Type ">";
separator nonempty Type ",";

Eor.                Expr        ::= Expr1 "||" Expr;
Eand.               Expr1       ::= Expr2 "&&" Expr1;
Ecmp.               Expr2       ::= Expr2 CmpOp Expr3;
Ecomplex.           Expr3       ::= ComplexVal;
Eadd.               Expr4       ::= Expr4 AddOp Expr5;
Emul.               Expr5       ::= Expr5 MulOp Expr6;
Eneg.               Expr6       ::= "-" Expr7;
Enot.               Expr6       ::= "!" Expr7;
Econst.             Expr7       ::= SimpleVal;              
EeasyMethod.        Expr8       ::= Expr9 "->" EasyMethod;
EhardMethod.        Expr9       ::= Expr9 "->" HardMethod;
EstringEl.          Expr10      ::= Expr10 "[" Expr "]";
Evariable.          Expr11      ::= Ident;
EexecFunc.          Expr11      ::= Ident "(" [Expr] ")";
coercions Expr 11;

CmpLe.              CmpOp       ::= "<";
CmpLeq.             CmpOp       ::= "<=";
CmpEq.              CmpOp       ::= "==";
CmpGeq.             CmpOp       ::= ">=";
CmpGe.              CmpOp       ::= ">";
CmpNeq.             CmpOp       ::= "!=";

Plus.               AddOp       ::= "+";
Minus.              AddOp       ::= "-";

Times.              MulOp       ::= "*";
Div.                MulOp       ::= "/";
Mod.                MulOp       ::= "%";

Size.               EasyMethod  ::= "size" "(" ")";
Empty.              EasyMethod  ::= "empty" "(" ")";

Get.                HardMethod  ::= "get" "(" Expr ")";
Back.               HardMethod  ::= "back" "(" ")";
Front.              HardMethod  ::= "front" "(" ")";

ConstInt.           SimpleVal   ::= Integer;
ConstBoolTrue.      SimpleVal   ::= "true";
ConstBoolFalse.     SimpleVal   ::= "false";
ConstString.        SimpleVal   ::= String;

ConstTuple.         ComplexVal  ::= "make_tuple" "(" [Expr] ")";
ConstList.          ComplexVal  ::= "list" "<" Type ">" "{" [Expr] "}";

separator Expr ",";

comment "//";
comment "/*" "*/";
```

---

##### **Kilka przykładowych programów**

```c++
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

  return 0;
}
```

```c++
tuple<bool, int> find_position(list<int> &numbers, int n) {
  if (numbers -> empty()) {
    return make_tuple(false, 0);
  }
  if (numbers->front() == n) {
    return make_tuple(true, 0);
  } else {
    numbers->pop_front();
    bool exist;
    int position_suf;
    tie(exist, position_suf) = find_position(numbers, n);
    return make_tuple(exist, 1 + position_suf);
  }
}

int main() {
  int n = 42; list<int> numbers;
  for (int i = 0; i <= n; i++) {
    numbers->push_back(i);
  }

  int m; bool ok;
  tie(ok, m) = find_position(numbers, n);
  if (!(ok && m == n && numbers -> empty())) {
    cout << "tu wejdziemy\n";
  }

  for (int i = 0; i < 1000; i++) {
    if (i < 42) {
      continue;
    }
    cout << "wypisze tylko " << i << "\n";
    break;
  }
  return 0;
}
```

---

##### **Tekstowy opis języka**

Język imperatywny, w składni opartej o podzbiór języków C/C++. Jest on lekko zmodyfikowany, tak by mógł spełniać założone funkcjonalności. Program składa się z deklaracji zmiennych/funkcji. Uruchomniony program wykona zadeklarowaną funkcję main. Główna funkcja musi być bezargumentowa, typu int i poprawnie wykonana zwraca wartość 0. Nie może być wywoływana w programie.

- **typy**: int, bool, string, tuple, list.

- **obsługiwane operacje** (arytmetyka, porównania i inne specyficzne operacje dla typu / zmiennej danego typu):
  
  | typ    | operacje                                                                              |
  |:------ |:------------------------------------------------------------------------------------- |
  | int    | +, -, *, /, %, <, <=, >=, >, ==, !=, ++, --, +=, -=, *=, /=, %=                       |
  | bool   | &&, \|\|, !, ==, !=, &&=, \|\|=                                                       |
  | string | +, ==, !=, +=, size, [], empty                                                        |
  | tuple  | ==, !=, get, make_tuple, tie                                                          |
  | list   | ==, !=, push_back, push_front, pop_back, pop_front, empty, front, back, list\<type>{} |
  
  Operacje =, +=, -=, ... przypisujące wartość, ale jej nie zwracają.
  
  **zmienne, operacja przypisania**
  
  Zmienne są określonego typu, który jest nadawany w momencie deklaracji. O ich wartościach początkowych nie należy nic zakładać, za wyjątkiem string'a i listy - są puste na start. Zmienne mogą być lolalne/globalne, być przysłaniane przez inne o tej samej nazwie. Przypisywanie będzie się odbywać przez operator= i nie może występować w wyrażeniu.
  
  Ponadto wywoływane metody np. size(), zamiast po "." będą po "->". 

- **jawne wypisywanie**
  
  Wypisywanie wartości na wyjście będzie możliwe poprzez intrukcję cout << .

- **funkcje i procedury z zagnieżdżaniem i rekurencją**
  
  Funkcje, nie licząc main'a, procedury - globalne i zagnieżdżone mogą zostać wywołane o ile były wcześniej zadeklarowane - identyfikatory będą statycznie związane, tak samo jak w przypadku zmiennych. Funkcje składają się z typu, nazwy, argumentów, ciała, zaś w ciele powinna znaleźć się intrukcja kończąca działanie return wraz z wynikiem. W przypadku procedur, ich typ jest void, natomiast intrukcja kończąca nie musi występować i jest to już return bez argumentu - wyniku.
  
  Przekazywane parametry mogą być przez zmienną/przez wartość. W przypadku przekazywania przez zmienną - w miejscu deklaracji funkcji/procedury wymagany jest pojedynczy znak & pomiędzy typem i parametrem/argumentem np. int &value.
  
  Niemożliwa jest odrębna definicja i deklaracja funkcji/procedury. Za to można je deklarować wewnątrz innych.
  
  ```C++
  int func(int a, int b); // niepoprawny zapis - odrębna definicja
  ...
  int func(int a, int b) {
    return a + b;
  }
  ```

- **pętle i intrukcje warunkowe**
  
  Dostępne będą while, if, jak również pętla for. W przypadku tych instrukcji, oczekiwane jest by w miejscu warunku była wartość typu bool np.:
  
  ```C++
  if (2 + 2) {...} // niepoprawne, 2 + 2 nie jest typu bool
  ```
  
  Pętla for jest postacji: for (deklaracja zmiennej; warunek; instrukcja modyfikująca zmienną) {...}

- **stylistyka ciał**
  
  Wszelkie ciała funkcji, procedur i instrukcji muszą mieć okalające nawiasy klamrowe, przykłady:
  
  ```c++
  int main() {...}
  void proc(...) {...}
  int func(...) {...}
  while(...) {...}
  if (...) {...}
  if (...) {...} else (...)
  for (...) {...}
  ```

- **obsługa błędów**
  
  Statyczne typowanie - faza kontroli typów i obsługa błędów wykonania - np. dzielenie przez zero. Program w wyniku pierwszego napotkanego błędu zostaje przerwany, a informacje o błędzie wypisywane.

---

##### **Wypełniona tabelka funkcjonalności**

```
Na 15 punktów
+ 01 (trzy typy)
+ 02 (literały, arytmetyka, porównania)
+ 03 (zmienne, przypisanie)
+ 04 (print)
+ 05 (while, if)
+ 06 (funkcje lub procedury, rekurencja)
+ 07 (przez zmienną / przez wartość / in/out) - zmienna, wartość
  08 (zmienne read-only i pętla for)
  Na 20 punktów
+ 09 (przesłanianie i statyczne wiązanie)
+ 10 (obsługa błędów wykonania)
+ 11 (funkcje zwracające wartość)
  Na 30 punktów
4 12 (4) (statyczne typowanie)
2 13 (2) (funkcje zagnieżdżone ze statycznym wiązaniem)
1 14 (1/2) (rekordy/listy/tablice/tablice wielowymiarowe) - listy
2 15 (2) (krotki z przypisaniem)
1 16 (1) (break, continue)
  17 (4) (funkcje wyższego rzędu, anonimowe, domknięcia)
  18 (3) (generatory)

Razem: 30
```