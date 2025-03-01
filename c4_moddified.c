// c4.c - C in four functions

// char, int, and pointer types
// if, while, return, and expression statements
// just enough features to allow self-compilation and a bit more

// Written by Robert Swierczek

#include <stdio.h>
#include <stdlib.h>
#include <memory.h>
#include <unistd.h>
#include <fcntl.h>
#define int long long

char *p, *lp, // current position in source code
     *data;   // data/bss pointer

int *e, *le,  // current position in emitted code
    *id,      // currently parsed identifier
    *sym,     // symbol table (simple list of identifiers)
    tk,       // current token
    ival,     // current token value
    ty,       // current expression type
    loc,      // local variable offset
    line,     // current line number
    src,      // print source and assembly flag
    debug;    // print executed instructions

// tokens and classes (operators last and in precedence order)
//MODIFICATION, AddMul +*  adds the first number to itself and then multiplies them together
enum {
  Num = 128, Fun, Sys, Glo, Loc, Id,
  Char, Else, Enum, If, Int, Return, Sizeof, While,
  Assign, Cond, Lor, Lan, Or, Xor, And, Eq, Ne, Lt, Gt, Le, Ge, Shl, Shr, Add, Sub, Mul, Div, Mod, Inc, Dec, Brak, AddMul
};

// opcodes
//MODIFICATION, Added DUP
enum { LEA ,IMM ,JMP ,JSR ,BZ  ,BNZ ,ENT ,ADJ ,LEV ,LI  ,LC  ,SI  ,SC  ,PSH ,
  OR  ,XOR ,AND ,EQ  ,NE  ,LT  ,GT  ,LE  ,GE  ,SHL ,SHR ,ADD ,SUB ,MUL ,DIV ,MOD ,
  OPEN,READ,CLOS,PRTF,MALC,FREE,MSET,MCMP,EXIT, DUP };

// types
enum { CHAR, INT, PTR };

// identifier offsets (since we can't create an ident struct)
enum { Tk, Hash, Name, Class, Type, Val, HClass, HType, HVal, Idsz };



// this function is responsible for analyzing the next character in the source code and identifying whether it represents a token in the programming language's syntax.
void next()
{
  char *pp; // A pointer for processing strings or characters.

  // the while loop runs until it encounters the end when it encounters 'tk = *p'
  while (tk = *p) {
    ++p; // moves the pointer to the next character
    // If the token is a new line, handle the new line and print source line information
    if (tk == '\n') {
      if (src) { // If source printing is wanted
        printf("%d: %.*s", line, p - lp, lp); // print the current line
        lp = p; // reset pointer to current position
        // print instructions from the opcode table starting with le
        while (le < e) {
          printf("%8.4s", &"LEA ,IMM ,JMP ,JSR ,BZ  ,BNZ ,ENT ,ADJ ,LEV ,LI  ,LC  ,SI  ,SC  ,PSH ,"
                           "OR  ,XOR ,AND ,EQ  ,NE  ,LT  ,GT  ,LE  ,GE  ,SHL ,SHR ,ADD ,SUB ,MUL ,DIV ,MOD ,"
                           "OPEN,READ,CLOS,PRTF,MALC,FREE,MSET,MCMP,EXIT,"[*++le * 5]);
          if (*le <= ADJ) printf(" %d\n", *++le); else printf("\n"); // if the le is less than or equal to the ADJ print the operand as a number, else justprint the opcode w/o operand
        }
      }
      ++line; // Increment line count
    }
    else if (tk == '#') { // check if comment if it is ignore it
      while (*p != 0 && *p != '\n') ++p; // put pointer at end of the comment
    }
    else if ((tk >= 'a' && tk <= 'z') || (tk >= 'A' && tk <= 'Z') || tk == '_') { // handle the identifiers
      pp = p - 1; // save the current position for the start of the identifier
      while ((*p >= 'a' && *p <= 'z') || (*p >= 'A' && *p <= 'Z') || (*p >= '0' && *p <= '9') || *p == '_')
        tk = tk * 147 + *p++;
      tk = (tk << 6) + (p - pp);
      id = sym;
      while (id[Tk]) {
        if (tk == id[Hash] && !memcmp((char *)id[Name], pp, p - pp)) { tk = id[Tk]; return; } // if you found a match return the token
        id = id + Idsz;
      }
      // if your identifier is new add it
      id[Name] = (int)pp;
      id[Hash] = tk;
      tk = id[Tk] = Id; // mark as identifier token type
      return;
    }
    // this one is to handle numbers
    else if (tk >= '0' && tk <= '9') { //numbers
      if (ival = tk - '0') { while (*p >= '0' && *p <= '9') ival = ival * 10 + *p++ - '0'; }
      else if (*p == 'x' || *p == 'X') { // hex numbers
        while ((tk = *++p) && ((tk >= '0' && tk <= '9') || (tk >= 'a' && tk <= 'f') || (tk >= 'A' && tk <= 'F')))
          ival = ival * 16 + (tk & 15) + (tk >= 'A' ? 9 : 0);
      }
      // octal numbers
      else { while (*p >= '0' && *p <= '7') ival = ival * 8 + *p++ - '0'; }
      tk = Num;
      return;
    }
    // Handle division and comments
    else if (tk == '/') {
      if (*p == '/') { // if // it's a comment
        ++p;
        while (*p != 0 && *p != '\n') ++p; // skip the comment
      }
      else { // if it's not a comment its division
        tk = Div;
        return;
      }
    }
    // this is to handle string literals
    else if (tk == '\'' || tk == '"') {
      pp = data; // storing string data
      while (*p != 0 && *p != tk) {  // process the characters inside the string
        if ((ival = *p++) == '\\') {
          if ((ival = *p++) == 'n') ival = '\n'; // handle new line
        }
        if (tk == '"') *data++ = ival; // store the character in string if it's a double-quoted string
      }
      ++p;  // to skio the closing quote
      if (tk == '"') ival = (int)pp; else tk = Num;
      return;
    } // multiple else ifs to handle ==, =, +, -, etc.
    else if (tk == '=') { if (*p == '=') { ++p; tk = Eq; } else tk = Assign; return; }
    else if (tk == '+') {  //MODIFICATION,  to recognize the +* operator, if not *+ the its +
      if (*p == '*') { 
        ++p; 
        tk = AddMul; 
      } else { 
        tk = Add; 
      } 
      return; 
    }
    else if (tk == '-') { if (*p == '-') { ++p; tk = Dec; } else tk = Sub; return; }
    else if (tk == '!') { if (*p == '=') { ++p; tk = Ne; } return; }
    else if (tk == '<') { if (*p == '=') { ++p; tk = Le; } else if (*p == '<') { ++p; tk = Shl; } else tk = Lt; return; }
    else if (tk == '>') { if (*p == '=') { ++p; tk = Ge; } else if (*p == '>') { ++p; tk = Shr; } else tk = Gt; return; }
    else if (tk == '|') { if (*p == '|') { ++p; tk = Lor; } else tk = Or; return; }
    else if (tk == '&') { if (*p == '&') { ++p; tk = Lan; } else tk = And; return; }
    else if (tk == '^') { tk = Xor; return; }
    else if (tk == '%') { tk = Mod; return; }
    else if (tk == '*') { tk = Mul; return; }
    else if (tk == '[') { tk = Brak; return; }
    else if (tk == '?') { tk = Cond; return; }
    // to ignore other characters that dont represent tokens
    else if (tk == '~' || tk == ';' || tk == '{' || tk == '}' || tk == '(' || tk == ')' || tk == ']' || tk == ',' || tk == ':') return;
  }
}

void expr(int lev)
{
  int t, *d; // declare the var

  if (!tk) { printf("%d: unexpected eof in expression\n", line); exit(-1); } // check if the token is empty, print error and exit
  else if (tk == Num) { *++e = IMM; *++e = ival; next(); ty = INT; }  // handle the number token
  else if (tk == '"') { // handle the string literal
    *++e = IMM; *++e = ival; next();
    while (tk == '"') next(); // skip any extra string tokens
    data = (char *)((int)data + sizeof(int) & -sizeof(int)); ty = PTR;
  }
  else if (tk == Sizeof) {  // handle the Sizeof operator
    next(); if (tk == '(') next(); else { printf("%d: open paren expected in sizeof\n", line); exit(-1); } // to ensure that parenthesis follows sizeof keyword
    ty = INT; if (tk == Int) next(); else if (tk == Char) { next(); ty = CHAR; } // check if its Int or Char
    while (tk == Mul) { next(); ty = ty + PTR; }  // handle the pointer tuypes
    if (tk == ')') next(); else { printf("%d: close paren expected in sizeof\n", line); exit(-1); } // make sure that there is a closing parenthesis
    *++e = IMM; *++e = (ty == CHAR) ? sizeof(char) : sizeof(int);    // push the size of the type into the stack
    ty = INT; // reset the type to INT
  }
  else if (tk == Id) {// handle the identifiers like variables and functions
    d = id; next();  // function call
    if (tk == '(') { // check if its a function call
      next();
      t = 0;// initialize counter
      while (tk != ')') { expr(Assign); *++e = PSH; ++t; if (tk == ',') next(); } // create a loop to handle arguments,so you evaluate each argument, push into stack, increase counter, move to next
      next();
      if (d[Class] == Sys) *++e = d[Val];  // for a system function, you push the system function address
      else if (d[Class] == Fun) { *++e = JSR; *++e = d[Val]; } // for a user-defined function, jump to func. then push func. adress
      else { printf("%d: bad function call\n", line); exit(-1); } // If it's neither of these print the error message
      if (t) { *++e = ADJ; *++e = t; } // if you hjave srguments adjust the stack and set the number
      ty = d[Type];
    }    // variables
    else if (d[Class] == Num) { *++e = IMM; *++e = d[Val]; ty = INT; } // for a constant number,  push to the stack, store, set type to INT
    else { // handle the local or global variables
      if (d[Class] == Loc) { *++e = LEA; *++e = loc - d[Val]; } // for a local variable, load the address,  calculate the address
      else if (d[Class] == Glo) { *++e = IMM; *++e = d[Val]; } // fir a global variable, push global adress, store global adress
      else { printf("%d: undefined variable\n", line); exit(-1); } // If its undefined print error
      *++e = ((ty = d[Type]) == CHAR) ? LC : LI;  // load the value based on its type
    }
  }// parentheses
  else if (tk == '(') { // for an open parenthesis
    next();
    if (tk == Int || tk == Char) { // check if its int or char
      t = (tk == Int) ? INT : CHAR; next();// set the type to int or char
      while (tk == Mul) { next(); t = t + PTR; } // handle pointers
      if (tk == ')') next(); else { printf("%d: bad cast\n", line); exit(-1); }
      expr(Inc); // evaluate expression inside parethisis
      ty = t;
    }
    else {
      expr(Assign); // evaluate expression
      if (tk == ')') next(); else { printf("%d: close paren expected\n", line); exit(-1); }  // move past the closing parenthesis, or give error
    }
  }
  else if (tk == Mul) { // dereference
    next(); expr(Inc);
    if (ty > INT) ty = ty - PTR; else { printf("%d: bad dereference\n", line); exit(-1); } // remove pointer level from type, else throw error
    *++e = (ty == CHAR) ? LC : LI; // load value
  }
  else if (tk == And) { // handling of &
    next(); expr(Inc); // evaluate
    if (*e == LC || *e == LI) --e; else { printf("%d: bad address-of\n", line); exit(-1); } // check if valid and adjust if not throw error
    ty = ty + PTR; // ddd the pointer level to the type
  }
  else if (tk == '!') { next(); expr(Inc); *++e = PSH; *++e = IMM; *++e = 0; *++e = EQ; ty = INT; } //logical NOT, checks if expression is false 0
  else if (tk == '~') { next(); expr(Inc); *++e = PSH; *++e = IMM; *++e = -1; *++e = XOR; ty = INT; }// bitwise NOT, flips all bits in the number
  else if (tk == Add) { next(); expr(Inc); ty = INT; } // does nothing, just moves to next token
  else if (tk == Sub) {  // makes number negative or multiplies by -1
    next(); *++e = IMM;
    if (tk == Num) { *++e = -ival; next(); } else { *++e = -1; *++e = PSH; expr(Inc); *++e = MUL; }
    ty = INT;
  }
  else if (tk == Inc || tk == Dec) { // preincrement (++x) or predecrement (--x): changes value before use
    t = tk; next(); expr(Inc);
    if (*e == LC) { *e = PSH; *++e = LC; } 
    else if (*e == LI) { *e = PSH; *++e = LI; }
    else { printf("%d: bad lvalue in pre-increment\n", line); exit(-1); }
    *++e = PSH;
    *++e = IMM; *++e = (ty > PTR) ? sizeof(int) : sizeof(char);
    *++e = (t == Inc) ? ADD : SUB; // add for ++ and subtract for --
    *++e = (ty == CHAR) ? SC : SI;
  }
  //MODIFICATION to handle the AddMul operator
  else if (tk == AddMul) { 
    next(); 
    *++e = PSH; 
    expr(Inc); 
    *++e = PSH; 
    *++e = DUP; 
    *++e = ADD; 
    *++e = MUL; 
    ty = INT; 
  }
  else { printf("%d: bad expression\n", line); exit(-1); } // error for inavlid expression

  while (tk >= lev) { // "precedence climbing" or "Top Down Operator Precedence" method
    t = ty;
    if (tk == Assign) { // = stores the right side value into the left side variable
      next();
      if (*e == LC || *e == LI) *e = PSH; else { printf("%d: bad lvalue in assignment\n", line); exit(-1); }
      expr(Assign); *++e = ((ty = t) == CHAR) ? SC : SI;
    }
    else if (tk == Cond) { // conditional operator ? chooses between two values
      next();
      *++e = BZ; d = ++e;
      expr(Assign);
      if (tk == ':') next(); else { printf("%d: conditional missing colon\n", line); exit(-1); }
      *d = (int)(e + 3); *++e = JMP; d = ++e;
      expr(Cond);
      *d = (int)(e + 1);
    }
    else if (tk == Lor) { next(); *++e = BNZ; d = ++e; expr(Lan); *d = (int)(e + 1); ty = INT; } //checks if one of the conditions is true
    else if (tk == Lan) { next(); *++e = BZ;  d = ++e; expr(Or);  *d = (int)(e + 1); ty = INT; }// checks if both conditions are true
    else if (tk == Or)  { next(); *++e = PSH; expr(Xor); *++e = OR;  ty = INT; }//performs bitwise OR between two numbers
    else if (tk == Xor) { next(); *++e = PSH; expr(And); *++e = XOR; ty = INT; }//performs bitwise XOR between two numbers
    else if (tk == And) { next(); *++e = PSH; expr(Eq);  *++e = AND; ty = INT; }//performs bitwise AND between two numbers
    else if (tk == Eq)  { next(); *++e = PSH; expr(Lt);  *++e = EQ;  ty = INT; }//checks if two values are equal
    else if (tk == Ne)  { next(); *++e = PSH; expr(Lt);  *++e = NE;  ty = INT; }//checks if two values are not equal
    else if (tk == Lt)  { next(); *++e = PSH; expr(Shl); *++e = LT;  ty = INT; }//checks if left value is smaller
    else if (tk == Gt)  { next(); *++e = PSH; expr(Shl); *++e = GT;  ty = INT; }//checks if left value is bigger
    else if (tk == Le)  { next(); *++e = PSH; expr(Shl); *++e = LE;  ty = INT; }// checks if left value is smaller or equal
    else if (tk == Ge)  { next(); *++e = PSH; expr(Shl); *++e = GE;  ty = INT; }//checks if left value is bigger or equal
    else if (tk == Shl) { next(); *++e = PSH; expr(Add); *++e = SHL; ty = INT; }//shifts bits to the left
    else if (tk == Shr) { next(); *++e = PSH; expr(Add); *++e = SHR; ty = INT; }//shifts bits to the right
    else if (tk == Add) { // adds the two values
      next(); *++e = PSH; expr(Mul);
      if ((ty = t) > PTR) { *++e = PSH; *++e = IMM; *++e = sizeof(int); *++e = MUL;  }
      *++e = ADD;
    }
    else if (tk == Sub) { //subtracts right value from left value
      next(); *++e = PSH; expr(Mul);
      if (t > PTR && t == ty) { *++e = SUB; *++e = PSH; *++e = IMM; *++e = sizeof(int); *++e = DIV; ty = INT; }
      else if ((ty = t) > PTR) { *++e = PSH; *++e = IMM; *++e = sizeof(int); *++e = MUL; *++e = SUB; }
      else *++e = SUB;
    }
    else if (tk == Mul) { next(); *++e = PSH; expr(Inc); *++e = MUL; ty = INT; }//multiplies the two values
    else if (tk == Div) { next(); *++e = PSH; expr(Inc); *++e = DIV; ty = INT; }// divides left value by right value
    else if (tk == Mod) { next(); *++e = PSH; expr(Inc); *++e = MOD; ty = INT; }//returns remainder
    else if (tk == Inc || tk == Dec) { // postincrement or postdecrement, changes value after use
      if (*e == LC) { *e = PSH; *++e = LC; }
      else if (*e == LI) { *e = PSH; *++e = LI; }
      else { printf("%d: bad lvalue in post-increment\n", line); exit(-1); }
      *++e = PSH; *++e = IMM; *++e = (ty > PTR) ? sizeof(int) : sizeof(char);
      *++e = (tk == Inc) ? ADD : SUB;
      *++e = (ty == CHAR) ? SC : SI;
      *++e = PSH; *++e = IMM; *++e = (ty > PTR) ? sizeof(int) : sizeof(char);
      *++e = (tk == Inc) ? SUB : ADD;
      next();
    }
    else if (tk == Brak) { // array indexing braket [] to access array elements
      next(); *++e = PSH; expr(Assign);
      if (tk == ']') next(); else { printf("%d: close bracket expected\n", line); exit(-1); }
      if (t > PTR) { *++e = PSH; *++e = IMM; *++e = sizeof(int); *++e = MUL;  } // calc offset for pointer arrays
      else if (t < PTR) { printf("%d: pointer type expected\n", line); exit(-1); }// give eerror if not a pointer
      *++e = ADD;
      *++e = ((ty = t - PTR) == CHAR) ? LC : LI;  // add offset and load
    }
    else { printf("%d: compiler error tk=%d\n", line, tk); exit(-1); } // error for unknown token
  }
}

void stmt()
{
  int *a, *b;

  if (tk == If) { // evaluates condition, executes statement if true, else optional statement
    next();
    if (tk == '(') next(); else { printf("%d: open paren expected\n", line); exit(-1); }
    expr(Assign);
    if (tk == ')') next(); else { printf("%d: close paren expected\n", line); exit(-1); }
    *++e = BZ; b = ++e; // if you get false jump to else or end
    stmt();
    if (tk == Else) { // handle if there is an else
      *b = (int)(e + 3); *++e = JMP; b = ++e;
      next();
      stmt();
    }
    *b = (int)(e + 1);
  }
  else if (tk == While) { //evaluates condition and repeats the statement while true
    next();
    next();
    a = e + 1; // save location to get back
    if (tk == '(') next(); else { printf("%d: open paren expected\n", line); exit(-1); }
    expr(Assign);
    if (tk == ')') next(); else { printf("%d: close paren expected\n", line); exit(-1); }
    *++e = BZ; b = ++e; // uf false quit loop
    stmt();
    *++e = JMP; *++e = (int)a; // get back to condition check
    *b = (int)(e + 1); ///finialize if cond. is false
  }
  else if (tk == Return) {//returns from function, optional expression
    next();
    if (tk != ';') expr(Assign);// return value
    *++e = LEV;//leave
    if (tk == ';') next(); else { printf("%d: semicolon expected\n", line); exit(-1); } //error
  }
  else if (tk == '{') { //executes the sequence of statements enclosed in braces
    next();
    while (tk != '}') stmt();// excute all inside
    next();
  }
  else if (tk == ';') { // a semicolon
    next();
  }
  else { //evaluates an expression followed by a semicolon
    expr(Assign);
    if (tk == ';') next(); else { printf("%d: semicolon expected\n", line); exit(-1); }
  }
}

int main(int argc, char **argv)
{
  int fd, bt, ty, poolsz, *idmain; //// file descriptor, byte size, type, memory pool size, main identifier
  int *pc, *sp, *bp, a, cycle; // vm registers
  int i, *t; // temps

  --argc; ++argv; //skip the program name argument
  if (argc > 0 && **argv == '-' && (*argv)[1] == 's') { src = 1; --argc; ++argv; }//check if the -s flag is set
  if (argc > 0 && **argv == '-' && (*argv)[1] == 'd') { debug = 1; --argc; ++argv; }// same for -d flag
  if (argc < 1) { printf("usage: c4 [-s] [-d] file ...\n"); return -1; }// error for no file

  if ((fd = open(*argv, 0)) < 0) { printf("could not open(%s)\n", *argv); return -1; }// open file

  poolsz = 256*1024; // arbitrary size 
  if (!(sym = malloc(poolsz))) { printf("could not malloc(%d) symbol area\n", poolsz); return -1; } // allocate memory for symbol area
  if (!(le = e = malloc(poolsz))) { printf("could not malloc(%d) text area\n", poolsz); return -1; }// text area
  if (!(data = malloc(poolsz))) { printf("could not malloc(%d) data area\n", poolsz); return -1; }// data area
  if (!(sp = malloc(poolsz))) { printf("could not malloc(%d) stack area\n", poolsz); return -1; }// stack area

  memset(sym,  0, poolsz); // initialize symbol area to zero
  memset(e,    0, poolsz);// text area
  memset(data, 0, poolsz);// data area

  p = "char else enum if int return sizeof while "
      "open read close printf malloc free memset memcmp exit void main"; // list of keywords, functions
  i = Char; while (i <= While) { next(); id[Tk] = i++; } // add keywords to symbol table
  i = OPEN; while (i <= EXIT) { next(); id[Class] = Sys; id[Type] = INT; id[Val] = i++; } // add library to symbol table
  next(); id[Tk] = Char; // handle void type
  next(); idmain = id; // keep track of main

  if (!(lp = p = malloc(poolsz))) { printf("could not malloc(%d) source area\n", poolsz); return -1; } // allocate memory for source area
  if ((i = read(fd, p, poolsz-1)) <= 0) { printf("read() returned %d\n", i); return -1; } // read file content into memory
  p[i] = 0;  // null terminate
  close(fd);

  // parse declarations
  line = 1;
  next();
  while (tk) {
    bt = INT; // basetype
    if (tk == Int) next(); // handle int type
    else if (tk == Char) { next(); bt = CHAR; }// handle char
    else if (tk == Enum) {// handle enum
      next();
      if (tk != '{') next(); // process enum values
      if (tk == '{') {
        next();
        i = 0;
        while (tk != '}') {
          if (tk != Id) { printf("%d: bad enum identifier %d\n", line, tk); return -1; }  // validate enum
          next();
          if (tk == Assign) { // handle the initializer
            next();
            if (tk != Num) { printf("%d: bad enum initializer\n", line); return -1; }
            i = ival;
            next();
          }
          id[Class] = Num; id[Type] = INT; id[Val] = i++; // set properties
          if (tk == ',') next();  // handle commas
        }
        next();
      }
    }
    while (tk != ';' && tk != '}') {
      ty = bt; // start with base type
      while (tk == Mul) { next(); ty = ty + PTR; }  // Hhndle pointer
      if (tk != Id) { printf("%d: bad global declaration\n", line); return -1; }  // check for valid identifier
      if (id[Class]) { printf("%d: duplicate global definition\n", line); return -1; } // check for duplicate
      next();
      id[Type] = ty;
      if (tk == '(') { // function
        id[Class] = Fun;
        id[Val] = (int)(e + 1);
        next(); i = 0;
        while (tk != ')') {
          ty = INT;
          if (tk == Int) next(); // handle int type in function parameters
          else if (tk == Char) { next(); ty = CHAR; } // handle char
          while (tk == Mul) { next(); ty = ty + PTR; } // handle pointer in parameter
          if (tk != Id) { printf("%d: bad parameter declaration\n", line); return -1; } // check for valid parameter identifier
          if (id[Class] == Loc) { printf("%d: duplicate parameter definition\n", line); return -1; } // Prevent duplicate para. definition
          id[HClass] = id[Class]; id[Class] = Loc;
          id[HType]  = id[Type];  id[Type] = ty;
          id[HVal]   = id[Val];   id[Val] = i++;
          next();
          if (tk == ',') next(); // handle multiple parameters between commas
        }
        next();
        if (tk != '{') { printf("%d: bad function definition\n", line); return -1; } // check for valid function body startand error if not
        loc = ++i; // track function local variables
        next();
        while (tk == Int || tk == Char) { // process local variable declarations
          bt = (tk == Int) ? INT : CHAR;
          next();
          while (tk != ';') {
            ty = bt;
            while (tk == Mul) { next(); ty = ty + PTR; } // handle pointer types
            if (tk != Id) { printf("%d: bad local declaration\n", line); return -1; } // check for valid local variable identifier
            if (id[Class] == Loc) { printf("%d: duplicate local definition\n", line); return -1; } // prevent duplicate local variable definition
            id[HClass] = id[Class]; id[Class] = Loc;
            id[HType]  = id[Type];  id[Type] = ty;
            id[HVal]   = id[Val];   id[Val] = ++i;
            next();
            if (tk == ',') next(); // handle multiple local variables
          }
          next();
        }
        *++e = ENT; *++e = i - loc; // set function entry
        while (tk != '}') stmt(); // execute body
        *++e = LEV; // set exit
        id = sym; // unwind symbol table locals
        while (id[Tk]) {
          if (id[Class] == Loc) {
            id[Class] = id[HClass];
            id[Type] = id[HType];
            id[Val] = id[HVal];
          }
          id = id + Idsz;
        }
      }
      else {  // global var. declaration
        id[Class] = Glo;
        id[Val] = (int)data;
        data = data + sizeof(int); // make space for global variable
      }
      if (tk == ',') next();  // handle multiple declarations
    }
    next();
  }

  if (!(pc = (int *)idmain[Val])) { printf("main() not defined\n"); return -1; } // check if main() exists
  if (src) return 0; // stop if source flag is set

  // setup stack
  bp = sp = (int *)((int)sp + poolsz);
  *--sp = EXIT; // call exit if main returns
  *--sp = PSH; t = sp;
  *--sp = argc; //arg count
  *--sp = (int)argv; // arg pointer
  *--sp = (int)t; // temp pointer

  // run...
  cycle = 0;
  while (1) {  // executes VM instructions until you exit or get errors
    i = *pc++; ++cycle;
    if (debug) { // MODIFICATION, added DUP
      printf("%d> %.4s", cycle,
        &"LEA ,IMM ,JMP ,JSR ,BZ  ,BNZ ,ENT ,ADJ ,LEV ,LI  ,LC  ,SI  ,SC  ,PSH ,"
         "OR  ,XOR ,AND ,EQ  ,NE  ,LT  ,GT  ,LE  ,GE  ,SHL ,SHR ,ADD ,SUB ,MUL ,DIV ,MOD ,"
         "OPEN,READ,CLOS,PRTF,MALC,FREE,MSET,MCMP,EXIT,DUP ,"[i * 5]);
      if (i <= ADJ) printf(" %d\n", *pc); else printf("\n");
    }
    if      (i == LEA) a = (int)(bp + *pc++);                             // load local address
    else if (i == IMM) a = *pc++;                                         // load global address or immediate
    else if (i == JMP) pc = (int *)*pc;                                   // jump
    else if (i == JSR) { *--sp = (int)(pc + 1); pc = (int *)*pc; }        // jump to subroutine
    else if (i == BZ)  pc = a ? pc + 1 : (int *)*pc;                      // branch if zero
    else if (i == BNZ) pc = a ? (int *)*pc : pc + 1;                      // branch if not zero
    else if (i == ENT) { *--sp = (int)bp; bp = sp; sp = sp - *pc++; }     // enter subroutine
    else if (i == ADJ) sp = sp + *pc++;                                   // stack adjust
    else if (i == LEV) { sp = bp; bp = (int *)*sp++; pc = (int *)*sp++; } // leave subroutine
    else if (i == LI)  a = *(int *)a;                                     // load int
    else if (i == LC)  a = *(char *)a;                                    // load char
    else if (i == SI)  *(int *)*sp++ = a;                                 // store int
    else if (i == SC)  a = *(char *)*sp++ = a;                            // store char
    else if (i == PSH) *--sp = a;                                         // push
    else if (i == DUP) {  *--sp = *sp; }//MODIFICATION, added DUP

    // for arithmetic and logic
    else if (i == OR)  a = *sp++ |  a; 
    else if (i == XOR) a = *sp++ ^  a;
    else if (i == AND) a = *sp++ &  a;
    else if (i == EQ)  a = *sp++ == a;
    else if (i == NE)  a = *sp++ != a;
    else if (i == LT)  a = *sp++ <  a;
    else if (i == GT)  a = *sp++ >  a;
    else if (i == LE)  a = *sp++ <= a;
    else if (i == GE)  a = *sp++ >= a;
    else if (i == SHL) a = *sp++ << a;
    else if (i == SHR) a = *sp++ >> a;
    else if (i == ADD) a = *sp++ +  a;
    else if (i == SUB) a = *sp++ -  a;
    else if (i == MUL) a = *sp++ *  a;
    else if (i == DIV) a = *sp++ /  a;
    else if (i == MOD) a = *sp++ %  a;

    // for system calls
    else if (i == OPEN) a = open((char *)sp[1], *sp);
    else if (i == READ) a = read(sp[2], (char *)sp[1], *sp);
    else if (i == CLOS) a = close(*sp);
    else if (i == PRTF) { t = sp + pc[1]; a = printf((char *)t[-1], t[-2], t[-3], t[-4], t[-5], t[-6]); }
    else if (i == MALC) a = (int)malloc(*sp);
    else if (i == FREE) free((void *)*sp);
    else if (i == MSET) a = (int)memset((char *)sp[2], sp[1], *sp);
    else if (i == MCMP) a = memcmp((char *)sp[2], (char *)sp[1], *sp);
    else if (i == EXIT) { printf("exit(%d) cycle = %d\n", *sp, cycle); return *sp; }
    else { printf("unknown instruction = %d! cycle = %d\n", i, cycle); return -1; }
  }
}
