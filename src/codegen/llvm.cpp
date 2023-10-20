#include "llvm.hpp"

using namespace llvm;

Type* Type::getInt32Ty(LLVMContext& context)
{
	return context.mInt32Ty;
}

ConstantInt* llvm::ConstantInt::get(Type* ty, uint64_t value, bool isSigned)
{
	return new ConstantInt(ty->mContext, APInt(ty->bits(), value, isSigned));
}

ConstantInt* llvm::ConstantInt::get(LLVMContext& context, const APInt& n)
{
	return new ConstantInt(context, n);
}

BasicBlock* llvm::BasicBlock::Create(LLVMContext& context, std::string name, Function* function)
{
	auto bb = new BasicBlock(context, name, function);
	if (function != nullptr) {
		function->mBlocks.push_back(bb);
	}
	return bb;
}

FunctionType* llvm::FunctionType::get(llvm::Type* ret, std::vector<llvm::Type*> args, bool varargs)
{
	return new FunctionType(ret, std::move(args), varargs);
}

Function* llvm::Function::Create(FunctionType* ft, Linkage linkage, std::string name, Module* module)
{
	auto fn = new Function(ft, linkage, std::move(name), module);
	module->mFunctions.push_back(fn);
	return fn;
}

void llvm::Argument::print(std::ostream& os)
{
	mType->print(os);
	os << ' ';
	printVar(os);
}

void llvm::IntegerType::print(std::ostream& os)
{
	os << 'i' << mSize;
}

void llvm::ConstantInt::print(std::ostream& os)
{
	__debugbreak();
}

void llvm::ConstantInt::printVar(std::ostream& os)
{
	os << 'i' << mValue.mBits << ' ' << mValue.mValue;
}

void llvm::AllocaInst::print(std::ostream& os)
{
	printVar(os);
	os << " = alloca ";
	mType->print(os);
	if (mAmount != nullptr) {
		os << ", ";
		mAmount->printVar(os);
	}
}

void llvm::BinOpInst::print(std::ostream& os)
{
	printVar(os);
	os << " = ";
	auto name = [this] {
		switch (mOp) {
		case BinOp::Add: return "Add";
		case BinOp::Sub: return "Sub";
		case BinOp::SDiv: return "SDiv";
		case BinOp::SRem: return "SRem";
		case BinOp::ICmpEQ: return "ICmpEQ";
		case BinOp::ICmpNE: return "ICmpNE";
		case BinOp::ICmpSLT: return "ICmpSLT";
		case BinOp::ICmpSLE: return "ICmpSLE";
		case BinOp::ICmpSGT: return "ICmpSGT";
		case BinOp::ICmpSGE: return "ICmpSGE";
		case BinOp::Or: return "Or";
		case BinOp::And: return "And";
		case BinOp::Mul: return "Mul";
		default: return "<fuck>";
		}
	}();
	os << name;
	os << ' ';
	mLhs->printVar(os);
	os << ", ";
	mRhs->printVar(os);
}

void llvm::LoadInst::print(std::ostream& os)
{
	// TODO: types
	printVar(os);
	os << " = load ";
	mPtr->printVar(os);
}

void llvm::StoreInst::print(std::ostream& os)
{
	// TODO: types
	os << "store ";
	mValue->printVar(os);
	os << ", ";
	mPtr->printVar(os);
}

void llvm::RetInst::print(std::ostream& os)
{
	os << "ret ";
	mValue->printVar(os);
}

void llvm::BrInst::print(std::ostream& os)
{
	os << "br label ";
	mDestination->printVar(os);
}

void llvm::CondBrInst::print(std::ostream& os)
{
	os << "condbr i1 ";
	mCondition->printVar(os);
	os << ", label ";
	mTrue->printVar(os);
	os << ", label ";
	mFalse->printVar(os);
}

void llvm::Value::printVar(std::ostream& os)
{
	if (mName.empty())
		mName = std::to_string(mContext.mUniqueId++);
	os << '%' << mName;
}

void llvm::BasicBlock::printVar(std::ostream& os)
{
	if (mName.empty())
		mName = std::to_string(mContext.mUniqueId++);
	os << '%' << mName;
}

void llvm::BasicBlock::print(std::ostream& os)
{
	printVar(os);
	os << ":\n";
	for (auto instr : mInstructions) {
		os << "  ";
		instr->print(os);
		os << '\n';
	}
}

void llvm::Function::print(std::ostream& os)
{
	mType->mRet->print(os);
	os << " @";
	os << mName;
	os << '(';
	for (size_t i = 0; i < mArgs.size(); i++)
	{
		if (i > 0)
			os << ", ";
		mArgs[i]->print(os);
	}
	os << ") {\n";
	for (auto block : mBlocks) {
		block->print(os);
		os << '\n';
	}
	os << "}\n";
}

void llvm::Module::print(std::ostream& os, void* nah)
{
	os << "; Module " << mName << "\n";
	for (auto& function : mFunctions) {
		function->print(os);
	}
}
