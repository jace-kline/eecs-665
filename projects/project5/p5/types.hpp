#ifndef XXLANG_DATA_TYPES
#define XXLANG_DATA_TYPES

#include <list>
#include <sstream>
#include "errors.hpp"

namespace holeyc{

// define a Kind type
enum Kind {VAR, FN, ERRKIND};

// define a Type type
enum Type {INT, INTPTR, BOOL, BOOLPTR, CHAR, CHARPTR, VOID, GENERIC, GENERICPTR, ERRTYPE};

// mapping of type to string representation
std::string typeStr(Type t);

// returns base type of a type
Type baseType(Type t);

// returns the pointer variant of a given type
// or ERRTYPE
Type refType(Type t);

// returns the base type of a given pointer type
// or ERRTYPE
Type derefType(Type t);

// returns true if the type is a pointer type
bool isPtrType(Type t);

// returns true if the type is bool, int, or char
bool isBaseType(Type t);

// define a DataType class
class DataType {
    public:
        DataType(Kind k, Type t)
        : kind(k), ret_type(t) {}
        Kind getKind() const { return kind; }
        Type getRetType() const { return ret_type; }
        bool isErrType() const { return (ret_type == ERRTYPE); }
        bool isFn() const { return kind == FN; }
        bool isConcrete() const { return kind == VAR; }
        bool isConcreteOf(Type t) const { return (kind == VAR && ret_type == t);}
        bool isBase() const { return isConcrete() && isBaseType(ret_type);}
        bool isPtr() const { return isPtrType(ret_type); }
        bool operator==(DataType& other) { 
            return (kind == other.kind && (ret_type == other.ret_type)); 
        }
        bool operator==(Type rhs) { return (ret_type == rhs); }
        bool operator!=(Type rhs) { return (ret_type != rhs); }

        virtual DataType * toRef() = 0;
        virtual DataType * toDeref() = 0;
    protected:
        Kind kind;
        Type ret_type;
};

class ErrType : public DataType {
    public:
        ErrType()
        : DataType(ERRKIND, ERRTYPE) {}
        DataType * toRef() override {return this;}
        DataType * toDeref() override {return this;}
};


// Represents a concrete type
class VarType : public DataType {
    public:
        VarType(Type t)
        : DataType(VAR, t) {}
        DataType * toRef() override {
            Type t = refType(ret_type);
            if(t == ERRTYPE) return new ErrType();
            return new VarType(t);
        }
        DataType * toDeref() override {
            Type t = derefType(ret_type);
            if(t == ERRTYPE) return new ErrType();
            return new VarType(t);
        }
};

// Represents a higher-order type
// Needs arguments filled to become concrete
class FnType : public DataType {
    public:
        FnType(Type t, std::list<Type> * ts)
        : DataType(FN, t), arg_types(ts) {}
        std::list<Type> * getArgTypes() { return arg_types; }
        DataType * toRef() override {return new ErrType();}
        DataType * toDeref() override {return new ErrType();}
    private:
        std::list<Type> * arg_types;
};

}

#endif
