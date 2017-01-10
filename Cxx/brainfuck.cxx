#include <iostream>
#include <fstream>
#include <vector>
#include <map>
#include <cstdint>
#include <cstring>
#include <iomanip>

/* stolen from http://stackoverflow.com/questions/3219393/stdlib-and-colored-output-in-c */

#define ANSI_COLOR_RED     "\x1b[31m"
#define ANSI_COLOR_GREEN   "\x1b[32m"
#define ANSI_COLOR_YELLOW  "\x1b[33m"
#define ANSI_COLOR_BLUE    "\x1b[34m"
#define ANSI_COLOR_MAGENTA "\x1b[35m"
#define ANSI_COLOR_CYAN    "\x1b[36m"
#define ANSI_COLOR_RESET   "\x1b[0m"

struct TapeC {
    int     local;
    uint8_t v[128];
    TapeC* next;
    TapeC* prev;
};

class Tape {
private:
    TapeC* curr;

    TapeC* newEmptyTapeC() {
        TapeC* x = new TapeC;
        x->local = 0;
        std::memset(x->v, 0, sizeof(x->v));
        x->next = x->prev = NULL;
        return x;
    }
    TapeC* next() {
        if (curr->next == NULL) {
            curr->next = newEmptyTapeC();
            curr->next->prev = curr;
        };
        return curr->next;
    }
    TapeC* prev() {
        if (curr->prev == NULL) {
            curr->prev = newEmptyTapeC();
            curr->prev->next = curr;
        };
        return curr->prev;
    }
    

public:
    Tape() {
        curr = newEmptyTapeC();
    }
    uint8_t currentValue() {
        return curr->v[curr->local];
    }
    void setCurrentValue(uint8_t x) {
        curr->v[curr->local] = x;
    }
    void next_cell() {
        curr->local++;
        if (curr->local >= 128) {
            curr = next();
            curr->local = 0;
        }
    }
    void prev_cell() {
        curr->local--;
        if (curr->local < 0) {
            curr = prev();
            curr->local = 127;
        }
    }
    void debug() {
        TapeC* start = curr;
        TapeC* end   = curr;
        while (start->prev) start = start->prev;
        while (end  ->next) end   = end  ->next;

        int start_of_tape = 0;
        for (int i=0; i<128; i++) {
            if (start->v[i] == 0) {
                start_of_tape = i+1;
            } else {
                break;
            }
        }
        int end_of_tape = 0;
        for (int i=0; i<128; i++) {
            if (end->v[i] != 0) {
                end_of_tape = i+1;
            }
        }
        if (start == curr && start_of_tape >= curr->local) {
            start_of_tape = curr->local - 1;
            start_of_tape = start_of_tape < 0 ? 0 : start_of_tape;
        }
        if (end == curr && end_of_tape <= curr->local) {
            end_of_tape = curr->local + 1;
            end_of_tape = end_of_tape > 255 ? 255 : end_of_tape;
        }


        TapeC* acc = start;
        std::cout << ANSI_COLOR_GREEN "Tape" ANSI_COLOR_MAGENTA ":" ANSI_COLOR_RESET " ...|";
        while (acc) {
            for (int i=(acc==start)?start_of_tape:0; (acc==end)?(i<end_of_tape):(i<128); i++){
                bool c = (acc == curr) && i == curr->local;
                std::cout << (c ? ANSI_COLOR_MAGENTA : "") << std::setw(3) << (int)acc->v[i]  << std::setw(0) << (c ? ANSI_COLOR_RESET "|" : "|");
            }
            acc = acc->next;
        }
        std::cout << "..." << std::endl;
    }
};

enum BF_OP_TAG {
        OpenLoop,
        CloseLoop,
        IORead,
        IOWrite,
        PtrIncrease,
        PtrDecrease,
        ValIncrease,
        ValDecrease,
#ifdef DEBUG
        ShowTape,
        Breakpoint,
#endif
};

struct InvalidBFConstruction {};

class BF_OP {
public:
    BF_OP_TAG tag;
    BF_OP() { tag = IOWrite; };
    BF_OP(BF_OP_TAG t) { tag = t; }

    std::string to_string() const {
        switch (tag) {
            case OpenLoop:
                return "[";
            case CloseLoop:
                return "]";
            case IORead:
                return ",";
            case IOWrite:
                return ".";
            case PtrIncrease:
                return ">";
            case PtrDecrease:
                return "<";
            case ValIncrease:
                return "+";
            case ValDecrease:
                return "-";
#ifdef DEBUG
            case ShowTape:
                return "#";
            case Breakpoint:
                return "$";
#endif
            default:
                return "WTF?";
        }
    }
};

struct UnmatchedBrackets {};

std::map<int,int> loopFinder(const std::vector<BF_OP>& program) {
    std::map<int,int> mp;
    std::vector<int> l;
    int ix = 0;
    while (ix < (int)program.size()) {
        switch (program[ix].tag) {
            case OpenLoop:
                l.push_back(ix);
                break;
            case CloseLoop:
                if (l.size() <= 0) throw UnmatchedBrackets();
                else {
                    int start = l[l.size()-1]; l.pop_back();
                    mp[start] = ix;
                    mp[ix]    = start;
                }
                break;
            default:
                break;
        }
        ix++;
    }
    if (l.size() > 0) throw UnmatchedBrackets();
    return mp;
}

void run(const std::vector<BF_OP>& program) {
    std::map<int,int> loops = loopFinder(program);
    Tape tape;

    int pc = 0;
#ifdef DEBUG
    bool step_by_step = false;
    std::string last = "n";
#endif
    while (pc < (int)program.size()) {
#ifdef DEBUG
        while (step_by_step) {
            std::cout << ANSI_COLOR_MAGENTA "pc=" ANSI_COLOR_RESET ANSI_COLOR_CYAN << std::setw(4) << pc << std::setw(0) << ANSI_COLOR_GREEN "|" ANSI_COLOR_RESET " ";
            for (int i=-20; i<=20; i++) {
                if ((pc+i) >= 0 && (pc+i) < (int)program.size()) {
                    std::cout << ((i == 0) ? ANSI_COLOR_RED : "" ) << program[pc+i].to_string() << ((i == 0) ? ANSI_COLOR_RESET : "");
                } else {
                    std::cout << " ";
                }
            }
            std::cout << ANSI_COLOR_GREEN " | " ANSI_COLOR_RESET "[tnrh]" ANSI_COLOR_MAGENTA ":" ANSI_COLOR_RESET " ";

            std::string x;
            getline(std::cin, x);
            if (x == "") x = last;
            else         last = x;
            if (x == "T" || x == "t") {
                tape.debug();
                continue;
             } else if (x == "N" || x == "n") {
                break;
             } else if (x == "R" || x == "r") {
                step_by_step = false;
                break;
             } else if (x == "H" || x == "h") {
                std::cout << ANSI_COLOR_GREEN "# " ANSI_COLOR_RESET "Debugger commands:" << std::endl;
                std::cout << "\t" ANSI_COLOR_MAGENTA "t" ANSI_COLOR_RESET " - show current tape" << std::endl;
                std::cout << "\t" ANSI_COLOR_MAGENTA "n" ANSI_COLOR_RESET " - advance one step on the evaluation" << std::endl;
                std::cout << "\t" ANSI_COLOR_MAGENTA "r" ANSI_COLOR_RESET " - go back to normal evaluation mode (will stop on next breakpoint)" << std::endl;
                std::cout << "\t" ANSI_COLOR_MAGENTA "h" ANSI_COLOR_RESET " - this help" << std::endl;
             }
        }
#endif
        switch (program[pc].tag) {
            case IORead:
                std::cout << "IORead_TODO" << std::endl;
                pc++;
                break;
            case IOWrite:
                std::cout << tape.currentValue();
                pc++;
                break;
            case PtrIncrease:
                tape.next_cell();
                pc++;
                break;
            case PtrDecrease:
                tape.prev_cell();
                pc++;
                break;
            case ValIncrease:
                tape.setCurrentValue(tape.currentValue() + 1);
                pc++;
                break;
            case ValDecrease:
                tape.setCurrentValue(tape.currentValue() - 1);
                pc++;
                break;
            case OpenLoop:
                if (tape.currentValue() != 0) {
                    pc++;
                } else {
                    pc = loops[pc]+1;
                }
                break;
            case CloseLoop:
                pc = loops[pc];
                break;
#ifdef DEBUG
            case ShowTape:
                tape.debug();
                pc++;
                break;
            case Breakpoint:
                step_by_step=true;
                pc++;
                break;
#endif
        }
    }
#ifdef DEBUG
    tape.debug();
#endif
}

std::vector<BF_OP> parse(const std::string& s) {
    std::map<char, BF_OP> opmap;
    opmap['+'] = BF_OP(ValIncrease);;
    opmap['-'] = BF_OP(ValDecrease);
    opmap['['] = BF_OP(OpenLoop);
    opmap[']'] = BF_OP(CloseLoop);
    opmap['>'] = BF_OP(PtrIncrease);
    opmap['<'] = BF_OP(PtrDecrease);
    opmap['.'] = BF_OP(IOWrite);
    opmap[','] = BF_OP(IORead);
#ifdef DEBUG
    opmap['#'] = BF_OP(ShowTape);
    opmap['$'] = BF_OP(Breakpoint);
#endif
    std::vector<BF_OP> v;
    for (char o : s) {
        auto i = opmap.find(o);
        if (i != opmap.end()) {
            v.push_back(i->second);
        }
    }
    return v;
}

int main(const int argc, const char * const * argv) {
    if (argc >= 1) {
        std::ifstream v;
        v.open(argv[1]);
        std::string str((std::istreambuf_iterator<char>(v)),
                         std::istreambuf_iterator<char>());
        run(parse(str));
        v.close();
    }
}
