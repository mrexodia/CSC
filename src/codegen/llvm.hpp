#pragma once

#include <map>
#include <string>
#include <vector>
#include <iostream>
#include <memory>

namespace llvm
{
    struct LLVMContext;

    struct Type {
        LLVMContext& mContext;

        explicit Type(LLVMContext& context) : mContext(context) { }

        static Type* getInt32Ty(LLVMContext& context);

        virtual int bits() const = 0;
        virtual void print(std::ostream& os) = 0;
    };

    struct IntegerType : Type {
        int mSize = 0;

        explicit IntegerType(LLVMContext& context, int size) : mSize(size), Type(context) { }

        int bits() const override { return mSize; }
        void print(std::ostream& os) override;
    };

    struct Value {
        virtual ~Value() = default;

        LLVMContext& mContext;
        std::string mName;

        explicit Value(LLVMContext& context) : mContext(context) { }

        void setName(std::string name) {
            mName = std::move(name);
        }

        virtual void print(std::ostream& os) = 0;
        virtual void printVar(std::ostream& os);
    };

    struct APInt {
        int mBits;
        uint64_t mValue;
        bool mIsSigned;

        APInt(int bits, uint64_t val, bool isSigned)
            : mBits(bits), mValue(val), mIsSigned(isSigned) {
        }
    };

    struct ConstantInt : Value {
        APInt mValue;

        explicit ConstantInt(LLVMContext& context, const APInt& value) : Value(context), mValue(value) {}

        static ConstantInt* get(Type* ty, uint64_t value, bool isSigned = true);
        static ConstantInt* get(LLVMContext& context, const APInt&);

        void print(std::ostream& os) override;
        void printVar(std::ostream& os) override;
    };

    struct Instruction : Value {
        explicit Instruction(LLVMContext& context) : Value(context) { }
    };

    struct AllocaInst : Instruction {
        Type* mType = nullptr;
        Value* mAmount = nullptr;

        AllocaInst(Type* type, Value* amount, std::string name) : mType(type), mAmount(amount), Instruction(type->mContext) {
            setName(std::move(name));
        }

        void print(std::ostream& os) override;
    };

    struct BinOpInst : Instruction {
        enum BinOp {
            Add,
            Sub,
            SDiv,
            SRem,
            ICmpEQ,
            ICmpNE,
            ICmpSLT,
            ICmpSLE,
            ICmpSGT,
            ICmpSGE,
            Or,
            And,
            Mul,
        } mOp;

        Value* mLhs = nullptr;
        Value* mRhs = nullptr;

        BinOpInst(BinOp op, Value* lhs, Value* rhs, std::string name) : mOp(op), mLhs(lhs), mRhs(rhs), Instruction(lhs->mContext) {
            setName(std::move(name));
        }

        void print(std::ostream& os) override;
    };

    struct LoadInst : Instruction {
        Value* mPtr = nullptr;

        LoadInst(Value* ptr, std::string name) : mPtr(ptr), Instruction(ptr->mContext) {
            setName(std::move(name));
        }

        void print(std::ostream& os) override;
    };

    struct StoreInst : Instruction {
        Value* mValue = nullptr;
        Value* mPtr = nullptr;

        StoreInst(Value* value, Value* ptr, std::string name) : mValue(value), mPtr(ptr), Instruction(value->mContext) {
            setName(std::move(name));
        }

        void print(std::ostream& os) override;
    };

    struct RetInst : Instruction {
        Value* mValue = nullptr;

        RetInst(Value* value, std::string name) : mValue(value), Instruction(value->mContext) {
            setName(std::move(name));
        }

        void print(std::ostream& os) override;
    };

    struct Function;

    struct BasicBlock {
        LLVMContext& mContext;
        std::string mName;
        Function* mParent = nullptr;
        
        BasicBlock(LLVMContext& context, std::string name, Function* parent) : mContext(context), mName(std::move(name)), mParent(parent) { }

        std::vector<Instruction*> mInstructions;

        using iterator = std::vector<Instruction*>::iterator;

        iterator begin() { return mInstructions.begin(); }

        static BasicBlock* Create(LLVMContext& context, std::string name, Function* function = nullptr);

        void printVar(std::ostream& os);
        void print(std::ostream& os);
    };

    struct BrInst : Instruction {
        BasicBlock* mDestination = nullptr;

        BrInst(BasicBlock* destination, std::string name) : mDestination(destination), Instruction(destination->mContext) {
            setName(std::move(name));
        }

        void print(std::ostream& os) override;
    };

    struct CondBrInst : Instruction {
        Value* mCondition = nullptr;
        BasicBlock* mTrue = nullptr;
        BasicBlock* mFalse = nullptr;

        CondBrInst(Value* condition, BasicBlock* tdest, BasicBlock* fdest, std::string name) : mCondition(condition), mTrue(tdest), mFalse(fdest), Instruction(condition->mContext) {
            setName(std::move(name));
        }

        void print(std::ostream& os) override;
    };

    struct FunctionType {
        llvm::Type* mRet = nullptr;
        std::vector<llvm::Type*> mArgs;
        bool mVarArgs = false;

        FunctionType(llvm::Type* ret, std::vector<llvm::Type*> args, bool varargs) : mRet(ret), mArgs(std::move(args)), mVarArgs(varargs) { }

        static FunctionType* get(llvm::Type* ret, std::vector<llvm::Type*> args, bool varargs);
    };

    struct Module;

    struct Argument : Value {
        Type* mType = nullptr;

        Argument(Type* type) : mType(type), Value(type->mContext) {}

        void print(std::ostream& os) override;
    };

    struct Function {
        enum Linkage {
            ExternalLinkage
        };

        FunctionType* mType;
        Linkage mLinkage;
        std::string mName;
        Module* mParent = nullptr;

        std::vector<Argument*> mArgs;
        std::vector<BasicBlock*> mBlocks;

        Function(FunctionType* type, Linkage linkage, std::string name, Module* module)
            : mType(type), mLinkage(linkage), mName(std::move(name)), mParent(module) {
            for (Type* argType : mType->mArgs) {
                mArgs.push_back(new Argument(argType));
            }
        }

        BasicBlock& getEntryBlock() {
            return *mBlocks.front();
        }

        std::vector<Argument*>& args() {
            return mArgs;
        }

        std::vector<BasicBlock*> getBasicBlockList() {
            return mBlocks;
        }

        static Function* Create(FunctionType* ft, Linkage linkage, std::string name, Module* module);

        void print(std::ostream& os);
    };

    inline std::ostream& outs() { return std::cout; }

    struct Module {
        LLVMContext& mContext;
        std::string mName;

        std::vector<Function*> mFunctions;

        Module(std::string name, LLVMContext& context)
            : mName(std::move(name)), mContext(context) { }

        void print(std::ostream& os, void* nah);
    };

    struct LLVMContext {
        std::vector<Module> mModules;
        Type* mInt32Ty = new IntegerType(*this, 32);
        int mUniqueId = 0;
    };

    template<class T = void>
    struct IRBuilder {
        LLVMContext& mContext;
        BasicBlock* mBlock = nullptr;
        BasicBlock::iterator mInsertPoint;

        explicit IRBuilder(LLVMContext& context):mContext(context) {
        }

        IRBuilder(BasicBlock* block, BasicBlock::iterator insert) : mContext(block->mContext), mBlock(block), mInsertPoint(insert) {
        }

        void SetInsertPoint(BasicBlock* insertBlock) {
            mBlock = insertBlock;
            mInsertPoint = insertBlock->begin();
        }

        template<class T>
        T* Insert(T* instr) {
            mBlock->mInstructions.push_back(instr);
            //mInsertPoint = mBlock->mInstructions.insert(mInsertPoint, instr);
            return instr;
        }

        Value* CreateAdd(Value* lhs, Value* rhs, std::string name = {}) { return Insert(new BinOpInst(BinOpInst::Add, lhs, rhs, std::move(name))); }
        Value* CreateSub(Value* lhs, Value* rhs, std::string name = {}) { return Insert(new BinOpInst(BinOpInst::Sub, lhs, rhs, std::move(name))); }
        Value* CreateSDiv(Value* lhs, Value* rhs, std::string name = {}) { return Insert(new BinOpInst(BinOpInst::SDiv, lhs, rhs, std::move(name))); }
        Value* CreateSRem(Value* lhs, Value* rhs, std::string name = {}) { return Insert(new BinOpInst(BinOpInst::SRem, lhs, rhs, std::move(name))); }
        Value* CreateICmpEQ(Value* lhs, Value* rhs, std::string name = {}) { return Insert(new BinOpInst(BinOpInst::ICmpEQ, lhs, rhs, std::move(name))); }
        Value* CreateICmpNE(Value* lhs, Value* rhs, std::string name = {}) { return Insert(new BinOpInst(BinOpInst::ICmpNE, lhs, rhs, std::move(name))); }
        Value* CreateICmpSLT(Value* lhs, Value* rhs, std::string name = {}) { return Insert(new BinOpInst(BinOpInst::ICmpSLT, lhs, rhs, std::move(name))); }
        Value* CreateICmpSLE(Value* lhs, Value* rhs, std::string name = {}) { return Insert(new BinOpInst(BinOpInst::ICmpSLE, lhs, rhs, std::move(name))); }
        Value* CreateICmpSGT(Value* lhs, Value* rhs, std::string name = {}) { return Insert(new BinOpInst(BinOpInst::ICmpSGT, lhs, rhs, std::move(name))); }
        Value* CreateICmpSGE(Value* lhs, Value* rhs, std::string name = {}) { return Insert(new BinOpInst(BinOpInst::ICmpSGE, lhs, rhs, std::move(name))); }
        Value* CreateOr(Value* lhs, Value* rhs, std::string name = {}) { return Insert(new BinOpInst(BinOpInst::Or, lhs, rhs, std::move(name))); }
        Value* CreateAnd(Value* lhs, Value* rhs, std::string name = {}) { return Insert(new BinOpInst(BinOpInst::And, lhs, rhs, std::move(name))); }
        Value* CreateMul(Value* lhs, Value* rhs, std::string name = {}) { return Insert(new BinOpInst(BinOpInst::Mul, lhs, rhs, std::move(name))); }
        AllocaInst* CreateAlloca(Type* ty, Value* v, std::string name = {}) { return Insert(new AllocaInst(ty, v, std::move(name))); }
        LoadInst* CreateLoad(Value* ptr, std::string name = {}) { return Insert(new LoadInst(ptr, std::move(name))); }
        StoreInst* CreateStore(Value* val, Value* ptr, std::string name = {}) { return Insert(new StoreInst(val, ptr, std::move(name))); }
        RetInst* CreateRet(Value* val, std::string name = {}) { return Insert(new RetInst(val, std::move(name))); }
        BrInst* CreateBr(BasicBlock* dest, std::string name = {}) { return Insert(new BrInst(dest, std::move(name))); }
        Instruction* CreateCondBr(Value* cond, BasicBlock* tdest, BasicBlock* fdest, std::string name = {}) { return Insert(new CondBrInst(cond, tdest, fdest, std::move(name))); }
    };
};