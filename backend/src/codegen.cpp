#include "llvm/ADT/APFloat.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/Verifier.h"
#include "llvm/Support/raw_ostream.h"
#include <algorithm>
#include <cctype>
#include <cstdio>
#include <cstdlib>
#include <iostream>
#include <map>
#include <memory>
#include <queue>
#include <string>
#include <vector>

using namespace llvm;

void LogError(const std::string &Str) {
    std::cout << Str;
    exit(1);
}

//===----------------------------------------------------------------------===//
// Abstract Syntax Tree (aka Parse Tree)
//===----------------------------------------------------------------------===//

namespace {

enum ArithOp {
    ADD,
    SUB,
    MUL,
    DIV,
    MOD,
};

enum CmpOp {
    EQ,
    NE,
    LT,
    GT,
    LE,
    GE,
};

enum LogicOp {
    AND,
    OR,
};

enum BitOp {
    BIT_AND,
    BIT_OR,
    BIT_XOR,
    BIT_LSHIFT,
    BIT_RSHIFT,
};

enum ExprType {
    INT,
    FLOAT,
    BOOL,
};

/// ExprAST - Base class for all expression nodes.
class ExprAST {
public:
    ExprType type;
    virtual ~ExprAST() = default;

    virtual Value *codegen() = 0;

    virtual void print() { std::cout << "ExprAST"; }
};

class FloatExprAST : public ExprAST {
    double Val;

public:
    FloatExprAST(double Val) : Val(Val) { type = FLOAT; }

    Value *codegen() override;

    void print() override { std::cout << Val; }
};

class IntExprAST : public ExprAST {
    int64_t Val;

public:
    IntExprAST(int64_t Val) : Val(Val) { type = INT; }

    Value *codegen() override;

    void print() override { std::cout << Val; }
};

class VariableExprAST : public ExprAST {
    std::string Name;

public:
    VariableExprAST(const std::string &Name, ExprType type) : Name(Name) {
        type = type;
    }

    Value *codegen() override;

    void print() override { std::cout << Name; }
};

class ArithOpExprAST : public ExprAST {
    ArithOp Op;
    std::unique_ptr<ExprAST> LHS, RHS;

public:
    ArithOpExprAST(ArithOp Op, std::unique_ptr<ExprAST> LHS,
                   std::unique_ptr<ExprAST> RHS)
        : Op(Op), LHS(std::move(LHS)), RHS(std::move(RHS)) {
        if (this->LHS->type == FLOAT || this->RHS->type == FLOAT) {
            type = FLOAT;
        } else {
            type = INT;
        }
    }

    Value *codegen() override;

    void print() override {
        std::cout << "(";
        LHS->print();
        switch (Op) {
        case ADD:
            std::cout << " + ";
            break;
        case SUB:
            std::cout << " - ";
            break;
        case MUL:
            std::cout << " * ";
            break;
        case DIV:
            std::cout << " / ";
            break;
        case MOD:
            std::cout << " % ";
            break;
        }
        RHS->print();
        std::cout << ")";
    }
};

class CmpOpExprAST : public ExprAST {
    CmpOp Op;
    std::unique_ptr<ExprAST> LHS, RHS;

public:
    CmpOpExprAST(CmpOp Op, std::unique_ptr<ExprAST> LHS,
                 std::unique_ptr<ExprAST> RHS)
        : Op(Op), LHS(std::move(LHS)), RHS(std::move(RHS)) {
        type = BOOL;
    }

    Value *codegen() override;

    void print() override {
        std::cout << "(";
        LHS->print();
        switch (Op) {
        case EQ:
            std::cout << " == ";
            break;
        case NE:
            std::cout << " != ";
            break;
        case LT:
            std::cout << " < ";
            break;
        case GT:
            std::cout << " > ";
            break;
        case LE:
            std::cout << " <= ";
            break;
        case GE:
            std::cout << " >= ";
            break;
        }
        RHS->print();
        std::cout << ")";
    }
};

class LogicOpExprAST : public ExprAST {
    LogicOp Op;
    std::unique_ptr<ExprAST> LHS, RHS;

public:
    LogicOpExprAST(LogicOp Op, std::unique_ptr<ExprAST> LHS,
                   std::unique_ptr<ExprAST> RHS)
        : Op(Op), LHS(std::move(LHS)), RHS(std::move(RHS)) {
        type = BOOL;
    }

    Value *codegen() override;

    void print() override {
        std::cout << "(";
        LHS->print();
        switch (Op) {
        case AND:
            std::cout << " && ";
            break;
        case OR:
            std::cout << " || ";
            break;
        }
        RHS->print();
        std::cout << ")";
    }
};

class BitOpExprAST : public ExprAST {
    BitOp Op;
    std::unique_ptr<ExprAST> LHS, RHS;

public:
    BitOpExprAST(BitOp Op, std::unique_ptr<ExprAST> LHS,
                 std::unique_ptr<ExprAST> RHS)
        : Op(Op), LHS(std::move(LHS)), RHS(std::move(RHS)) {
        type = INT;
    }

    Value *codegen() override;

    void print() override {
        std::cout << "(";
        LHS->print();
        switch (Op) {
        case BIT_LSHIFT:
            std::cout << " << ";
            break;
        case BIT_RSHIFT:
            std::cout << " >> ";
            break;
        case BIT_AND:
            std::cout << " & ";
            break;
        case BIT_OR:
            std::cout << " | ";
            break;
        case BIT_XOR:
            std::cout << " ^ ";
            break;
        }
        RHS->print();
        std::cout << ")";
    }
};

class PrintExprAST : public ExprAST {
    std::unique_ptr<ExprAST> Arg;

public:
    PrintExprAST(std::unique_ptr<ExprAST> Arg) : Arg(std::move(Arg)) {}

    void print() override {
        std::cout << "print(";
        Arg->print();
        std::cout << ")";
    }

    Value *codegen() override;
};

} // end anonymous namespace

//===----------------------------------------------------------------------===//
// Code Generation
//===----------------------------------------------------------------------===//

static std::unique_ptr<LLVMContext> TheContext;
static std::unique_ptr<Module> TheModule;
static std::unique_ptr<IRBuilder<>> Builder;
static std::map<std::string, Value *> NamedValues;

Value *LogErrorV(const char *Str) {
    LogError(Str);
    return nullptr;
}

Value *FloatExprAST::codegen() {
    return ConstantFP::get(*TheContext, APFloat(Val));
}

Value *IntExprAST::codegen() {
    return ConstantInt::get(*TheContext, APInt(64, Val));
}

Value *VariableExprAST::codegen() {
    // Look this variable up in the function.
    Value *V = NamedValues[Name];
    if (!V) return LogErrorV("Unknown variable name");
    return V;
}

Value *ArithOpExprAST::codegen() {
    Value *L = LHS->codegen();
    Value *R = RHS->codegen();
    if (!L || !R) return nullptr;

    switch (Op) {
    case ADD:
        if (type == INT) {
            return Builder->CreateAdd(L, R, "addtmp");
        } else {
            return Builder->CreateFAdd(L, R, "addtmp");
        }
    case SUB:
        if (type == INT) {
            return Builder->CreateSub(L, R, "subtmp");
        } else {
            return Builder->CreateFSub(L, R, "subtmp");
        }
    case MUL:
        if (type == INT) {
            return Builder->CreateMul(L, R, "multmp");
        } else {
            return Builder->CreateFMul(L, R, "multmp");
        }
    case DIV:
        if (type == INT) {
            return Builder->CreateSDiv(L, R, "divtmp");
        } else {
            return Builder->CreateFDiv(L, R, "divtmp");
        }
    case MOD:
        if (type == INT) {
            return Builder->CreateSRem(L, R, "modtmp");
        } else {
            LogErrorV("Modulo operator only works on integers");
        }
    }
    LogErrorV("Invalid arithmetic operator");
}

Value *CmpOpExprAST::codegen() {
    Value *L = LHS->codegen();
    Value *R = RHS->codegen();
    if (!L || !R) return nullptr;

    switch (Op) {
    case EQ:
        if (LHS->type == FLOAT || RHS->type == FLOAT) {
            return Builder->CreateFCmpOEQ(L, R, "eqtmp");
        } else {
            return Builder->CreateICmpEQ(L, R, "eqtmp");
        }
    case NE:
        if (LHS->type == FLOAT || RHS->type == FLOAT) {
            return Builder->CreateFCmpONE(L, R, "netmp");
        } else {
            return Builder->CreateICmpNE(L, R, "netmp");
        }
    case LT:
        if (LHS->type == FLOAT || RHS->type == FLOAT) {
            return Builder->CreateFCmpOLT(L, R, "lttmp");
        } else {
            return Builder->CreateICmpSLT(L, R, "lttmp");
        }
    case GT:
        if (LHS->type == FLOAT || RHS->type == FLOAT) {
            return Builder->CreateFCmpOGT(L, R, "gttmp");
        } else {
            return Builder->CreateICmpSGT(L, R, "gttmp");
        }
    case LE:
        if (LHS->type == FLOAT || RHS->type == FLOAT) {
            return Builder->CreateFCmpOLE(L, R, "letmp");
        } else {
            return Builder->CreateICmpSLE(L, R, "letmp");
        }
    case GE:
        if (LHS->type == FLOAT || RHS->type == FLOAT) {
            return Builder->CreateFCmpOGE(L, R, "getmp");
        } else {
            return Builder->CreateICmpSGE(L, R, "getmp");
        }
    }
    LogErrorV("Invalid comparison operator");
}

Value *LogicOpExprAST::codegen() {
    Value *L = LHS->codegen();
    Value *R = RHS->codegen();
    if (!L || !R) return nullptr;
    if (LHS->type != BOOL || RHS->type != BOOL) {
        LogErrorV("Logical operators only work on integers");
    }

    switch (Op) {
    case AND:
        return Builder->CreateAnd(L, R, "andtmp");
    case OR:
        return Builder->CreateOr(L, R, "ortmp");
    }
    LogErrorV("Invalid logical operator");
}

Value *BitOpExprAST::codegen() {
    Value *L = LHS->codegen();
    Value *R = RHS->codegen();
    if (!L || !R) return nullptr;
    if (LHS->type != INT || RHS->type != INT) {
        LogErrorV("Bitwise operators only work on integers");
    }

    switch (Op) {
    case BIT_AND:
        return Builder->CreateAnd(L, R, "andtmp");
    case BIT_OR:
        return Builder->CreateOr(L, R, "ortmp");
    case BIT_XOR:
        return Builder->CreateXor(L, R, "xortmp");
    case BIT_LSHIFT:
        return Builder->CreateShl(L, R, "lshifttmp");
    case BIT_RSHIFT:
        return Builder->CreateLShr(L, R, "rshifttmp");
    }
    LogErrorV("Invalid bit operator");
}

//===----------------------------------------------------------------------===//
// Top-Level parsing and JIT Driver
//===----------------------------------------------------------------------===//

static void InitializeModule() {
    // Open a new context and module.
    TheContext = std::make_unique<LLVMContext>();
    TheModule = std::make_unique<Module>("kuhu_source", *TheContext);

    // Create a new builder for the module.
    Builder = std::make_unique<IRBuilder<>>(*TheContext);
}

//===----------------------------------------------------------------------===//
// Main driver code.
//===----------------------------------------------------------------------===//

ExprAST *random_expr() {
    const std::vector<BitOp> bitops = {
        BIT_AND, BIT_OR, BIT_XOR, BIT_LSHIFT, BIT_RSHIFT,
    };
    const std::vector<ArithOp> arithOps = {
        ADD, SUB, MUL, DIV, MOD,
    };

    const std::vector<CmpOp> cmpOps = {
        EQ, NE, LT, GT, LE, GE,
    };

    const std::vector<LogicOp> logicOps = {
        AND,
        OR,
    };

    enum OpType { LOGIC, CMP, ARITH, BIT };

    auto random_arith = [arithOps]() -> std::unique_ptr<ExprAST> {
        std::unique_ptr<ExprAST> LHS =
            std::make_unique<IntExprAST>(rand() % 100);
        std::unique_ptr<ExprAST> RHS =
            std::make_unique<IntExprAST>(rand() % 100);
        auto res = std::make_unique<ArithOpExprAST>(
            arithOps[rand() % arithOps.size()], std::move(LHS), std::move(RHS));
        return res;
    };

    auto random_cmp = [cmpOps,
                       random_arith]() -> std::unique_ptr<CmpOpExprAST> {
        auto LHS = random_arith();
        auto RHS = random_arith();
        auto res = std::make_unique<CmpOpExprAST>(
            cmpOps[rand() % cmpOps.size()], std::move(LHS), std::move(RHS));
        return res;
    };

    auto random_logic = [logicOps,
                         random_cmp]() -> std::unique_ptr<LogicOpExprAST> {
        auto LHS = random_cmp();
        auto RHS = random_cmp();
        auto res = std::make_unique<LogicOpExprAST>(
            logicOps[rand() % logicOps.size()], std::move(LHS), std::move(RHS));
        return res;
    };

    auto random_bit = [bitops,
                       random_arith]() -> std::unique_ptr<BitOpExprAST> {
        auto LHS = random_arith();
        auto RHS = random_arith();
        auto res = std::make_unique<BitOpExprAST>(
            bitops[rand() % bitops.size()], std::move(LHS), std::move(RHS));
        return res;
    };

    auto bitopExpr = random_bit();
    auto logicExpr = random_logic();

    auto res = std::make_unique<CmpOpExprAST>(cmpOps[rand() % cmpOps.size()],
                                              std::move(bitopExpr),
                                              std::move(logicExpr));
    return res.release();
}

void test() {
    std::string fnname = "main";

    auto voidtype = Type::getVoidTy(*TheContext);
    FunctionType *FT = FunctionType::get(voidtype, voidtype);

    Function *F = Function::Create(FT, Function::ExternalLinkage, fnname,
                                   TheModule.get());

    BasicBlock *BB = BasicBlock::Create(*TheContext, "entry", F);
    Builder->SetInsertPoint(BB);

    ExprAST *Expr = random_expr();

    Expr->print();
    std::cout << "\n\n";

    auto RetVal = Expr->codegen();
    Builder->CreateRet(RetVal);

    verifyFunction(*F);
}

int main() {

    // Make the module, which holds all the code.
    InitializeModule();

    // Code
    test();

    // Print out all of the generated code.
    TheModule->print(errs(), nullptr);

    return 0;
}