// A Bison parser, made by GNU Bison 3.5.1.

// Skeleton implementation for Bison LALR(1) parsers in C++

// Copyright (C) 2002-2015, 2018-2020 Free Software Foundation, Inc.

// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.

// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.

// As a special exception, you may create a larger work that contains
// part or all of the Bison parser skeleton and distribute that work
// under terms of your choice, so long as that work isn't itself a
// parser generator using the skeleton or a modified version thereof
// as a parser skeleton.  Alternatively, if you modify or redistribute
// the parser skeleton itself, you may (at your option) remove this
// special exception, which will cause the skeleton and the resulting
// Bison output files to be licensed under the GNU General Public
// License without this special exception.

// This special exception was added by the Free Software Foundation in
// version 2.2 of Bison.

// Undocumented macros, especially those whose name start with YY_,
// are private implementation details.  Do not rely on them.





#include "grammar.hh"


// Unqualified %code blocks.
#line 42 "holyc.yy"

   // C std code for utility functions
   #include <iostream>
   #include <cstdlib>
   #include <fstream>

   // Our code for interoperation between scanner/parser
   #include "scanner.hpp"

   // Include our ParseTree class
   // #include "parsetree.hpp"

  //Request tokens from our scanner member, not 
  // from a global function
  #undef yylex
  #define yylex scanner.yylex

#line 63 "parser.cc"


#ifndef YY_
# if defined YYENABLE_NLS && YYENABLE_NLS
#  if ENABLE_NLS
#   include <libintl.h> // FIXME: INFRINGES ON USER NAME SPACE.
#   define YY_(msgid) dgettext ("bison-runtime", msgid)
#  endif
# endif
# ifndef YY_
#  define YY_(msgid) msgid
# endif
#endif

// Whether we are compiled with exception support.
#ifndef YY_EXCEPTIONS
# if defined __GNUC__ && !defined __EXCEPTIONS
#  define YY_EXCEPTIONS 0
# else
#  define YY_EXCEPTIONS 1
# endif
#endif



// Enable debugging if requested.
#if YYDEBUG

// A pseudo ostream that takes yydebug_ into account.
# define YYCDEBUG if (yydebug_) (*yycdebug_)

# define YY_SYMBOL_PRINT(Title, Symbol)         \
  do {                                          \
    if (yydebug_)                               \
    {                                           \
      *yycdebug_ << Title << ' ';               \
      yy_print_ (*yycdebug_, Symbol);           \
      *yycdebug_ << '\n';                       \
    }                                           \
  } while (false)

# define YY_REDUCE_PRINT(Rule)          \
  do {                                  \
    if (yydebug_)                       \
      yy_reduce_print_ (Rule);          \
  } while (false)

# define YY_STACK_PRINT()               \
  do {                                  \
    if (yydebug_)                       \
      yystack_print_ ();                \
  } while (false)

#else // !YYDEBUG

# define YYCDEBUG if (false) std::cerr
# define YY_SYMBOL_PRINT(Title, Symbol)  YYUSE (Symbol)
# define YY_REDUCE_PRINT(Rule)           static_cast<void> (0)
# define YY_STACK_PRINT()                static_cast<void> (0)

#endif // !YYDEBUG

#define yyerrok         (yyerrstatus_ = 0)
#define yyclearin       (yyla.clear ())

#define YYACCEPT        goto yyacceptlab
#define YYABORT         goto yyabortlab
#define YYERROR         goto yyerrorlab
#define YYRECOVERING()  (!!yyerrstatus_)

#line 5 "holyc.yy"
namespace holyc {
#line 136 "parser.cc"


  /* Return YYSTR after stripping away unnecessary quotes and
     backslashes, so that it's suitable for yyerror.  The heuristic is
     that double-quoting is unnecessary unless the string contains an
     apostrophe, a comma, or backslash (other than backslash-backslash).
     YYSTR is taken from yytname.  */
  std::string
  Parser::yytnamerr_ (const char *yystr)
  {
    if (*yystr == '"')
      {
        std::string yyr;
        char const *yyp = yystr;

        for (;;)
          switch (*++yyp)
            {
            case '\'':
            case ',':
              goto do_not_strip_quotes;

            case '\\':
              if (*++yyp != '\\')
                goto do_not_strip_quotes;
              else
                goto append;

            append:
            default:
              yyr += *yyp;
              break;

            case '"':
              return yyr;
            }
      do_not_strip_quotes: ;
      }

    return yystr;
  }


  /// Build a parser object.
  Parser::Parser (holyc::Scanner &scanner_yyarg)
#if YYDEBUG
    : yydebug_ (false),
      yycdebug_ (&std::cerr),
#else
    :
#endif
      scanner (scanner_yyarg)
  {}

  Parser::~Parser ()
  {}

  Parser::syntax_error::~syntax_error () YY_NOEXCEPT YY_NOTHROW
  {}

  /*---------------.
  | Symbol types.  |
  `---------------*/

  // basic_symbol.
#if 201103L <= YY_CPLUSPLUS
  template <typename Base>
  Parser::basic_symbol<Base>::basic_symbol (basic_symbol&& that)
    : Base (std::move (that))
    , value (std::move (that.value))
  {}
#endif

  template <typename Base>
  Parser::basic_symbol<Base>::basic_symbol (const basic_symbol& that)
    : Base (that)
    , value (that.value)
  {}


  /// Constructor for valueless symbols.
  template <typename Base>
  Parser::basic_symbol<Base>::basic_symbol (typename Base::kind_type t)
    : Base (t)
    , value ()
  {}

  template <typename Base>
  Parser::basic_symbol<Base>::basic_symbol (typename Base::kind_type t, YY_RVREF (semantic_type) v)
    : Base (t)
    , value (YY_MOVE (v))
  {}

  template <typename Base>
  bool
  Parser::basic_symbol<Base>::empty () const YY_NOEXCEPT
  {
    return Base::type_get () == empty_symbol;
  }

  template <typename Base>
  void
  Parser::basic_symbol<Base>::move (basic_symbol& s)
  {
    super_type::move (s);
    value = YY_MOVE (s.value);
  }

  // by_type.
  Parser::by_type::by_type ()
    : type (empty_symbol)
  {}

#if 201103L <= YY_CPLUSPLUS
  Parser::by_type::by_type (by_type&& that)
    : type (that.type)
  {
    that.clear ();
  }
#endif

  Parser::by_type::by_type (const by_type& that)
    : type (that.type)
  {}

  Parser::by_type::by_type (token_type t)
    : type (yytranslate_ (t))
  {}

  void
  Parser::by_type::clear ()
  {
    type = empty_symbol;
  }

  void
  Parser::by_type::move (by_type& that)
  {
    type = that.type;
    that.clear ();
  }

  int
  Parser::by_type::type_get () const YY_NOEXCEPT
  {
    return type;
  }


  // by_state.
  Parser::by_state::by_state () YY_NOEXCEPT
    : state (empty_state)
  {}

  Parser::by_state::by_state (const by_state& that) YY_NOEXCEPT
    : state (that.state)
  {}

  void
  Parser::by_state::clear () YY_NOEXCEPT
  {
    state = empty_state;
  }

  void
  Parser::by_state::move (by_state& that)
  {
    state = that.state;
    that.clear ();
  }

  Parser::by_state::by_state (state_type s) YY_NOEXCEPT
    : state (s)
  {}

  Parser::symbol_number_type
  Parser::by_state::type_get () const YY_NOEXCEPT
  {
    if (state == empty_state)
      return empty_symbol;
    else
      return yystos_[+state];
  }

  Parser::stack_symbol_type::stack_symbol_type ()
  {}

  Parser::stack_symbol_type::stack_symbol_type (YY_RVREF (stack_symbol_type) that)
    : super_type (YY_MOVE (that.state), YY_MOVE (that.value))
  {
#if 201103L <= YY_CPLUSPLUS
    // that is emptied.
    that.state = empty_state;
#endif
  }

  Parser::stack_symbol_type::stack_symbol_type (state_type s, YY_MOVE_REF (symbol_type) that)
    : super_type (s, YY_MOVE (that.value))
  {
    // that is emptied.
    that.type = empty_symbol;
  }

#if YY_CPLUSPLUS < 201103L
  Parser::stack_symbol_type&
  Parser::stack_symbol_type::operator= (const stack_symbol_type& that)
  {
    state = that.state;
    value = that.value;
    return *this;
  }

  Parser::stack_symbol_type&
  Parser::stack_symbol_type::operator= (stack_symbol_type& that)
  {
    state = that.state;
    value = that.value;
    // that is emptied.
    that.state = empty_state;
    return *this;
  }
#endif

  template <typename Base>
  void
  Parser::yy_destroy_ (const char* yymsg, basic_symbol<Base>& yysym) const
  {
    if (yymsg)
      YY_SYMBOL_PRINT (yymsg, yysym);

    // User destructor.
    YYUSE (yysym.type_get ());
  }

#if YYDEBUG
  template <typename Base>
  void
  Parser::yy_print_ (std::ostream& yyo,
                                     const basic_symbol<Base>& yysym) const
  {
    std::ostream& yyoutput = yyo;
    YYUSE (yyoutput);
    symbol_number_type yytype = yysym.type_get ();
#if defined __GNUC__ && ! defined __clang__ && ! defined __ICC && __GNUC__ * 100 + __GNUC_MINOR__ <= 408
    // Avoid a (spurious) G++ 4.8 warning about "array subscript is
    // below array bounds".
    if (yysym.empty ())
      std::abort ();
#endif
    yyo << (yytype < yyntokens_ ? "token" : "nterm")
        << ' ' << yytname_[yytype] << " (";
    YYUSE (yytype);
    yyo << ')';
  }
#endif

  void
  Parser::yypush_ (const char* m, YY_MOVE_REF (stack_symbol_type) sym)
  {
    if (m)
      YY_SYMBOL_PRINT (m, sym);
    yystack_.push (YY_MOVE (sym));
  }

  void
  Parser::yypush_ (const char* m, state_type s, YY_MOVE_REF (symbol_type) sym)
  {
#if 201103L <= YY_CPLUSPLUS
    yypush_ (m, stack_symbol_type (s, std::move (sym)));
#else
    stack_symbol_type ss (s, sym);
    yypush_ (m, ss);
#endif
  }

  void
  Parser::yypop_ (int n)
  {
    yystack_.pop (n);
  }

#if YYDEBUG
  std::ostream&
  Parser::debug_stream () const
  {
    return *yycdebug_;
  }

  void
  Parser::set_debug_stream (std::ostream& o)
  {
    yycdebug_ = &o;
  }


  Parser::debug_level_type
  Parser::debug_level () const
  {
    return yydebug_;
  }

  void
  Parser::set_debug_level (debug_level_type l)
  {
    yydebug_ = l;
  }
#endif // YYDEBUG

  Parser::state_type
  Parser::yy_lr_goto_state_ (state_type yystate, int yysym)
  {
    int yyr = yypgoto_[yysym - yyntokens_] + yystate;
    if (0 <= yyr && yyr <= yylast_ && yycheck_[yyr] == yystate)
      return yytable_[yyr];
    else
      return yydefgoto_[yysym - yyntokens_];
  }

  bool
  Parser::yy_pact_value_is_default_ (int yyvalue)
  {
    return yyvalue == yypact_ninf_;
  }

  bool
  Parser::yy_table_value_is_error_ (int yyvalue)
  {
    return yyvalue == yytable_ninf_;
  }

  int
  Parser::operator() ()
  {
    return parse ();
  }

  int
  Parser::parse ()
  {
    int yyn;
    /// Length of the RHS of the rule being reduced.
    int yylen = 0;

    // Error handling.
    int yynerrs_ = 0;
    int yyerrstatus_ = 0;

    /// The lookahead symbol.
    symbol_type yyla;

    /// The return value of parse ().
    int yyresult;

#if YY_EXCEPTIONS
    try
#endif // YY_EXCEPTIONS
      {
    YYCDEBUG << "Starting parse\n";


    /* Initialize the stack.  The initial state will be set in
       yynewstate, since the latter expects the semantical and the
       location values to have been already stored, initialize these
       stacks with a primary value.  */
    yystack_.clear ();
    yypush_ (YY_NULLPTR, 0, YY_MOVE (yyla));

  /*-----------------------------------------------.
  | yynewstate -- push a new symbol on the stack.  |
  `-----------------------------------------------*/
  yynewstate:
    YYCDEBUG << "Entering state " << int (yystack_[0].state) << '\n';

    // Accept?
    if (yystack_[0].state == yyfinal_)
      YYACCEPT;

    goto yybackup;


  /*-----------.
  | yybackup.  |
  `-----------*/
  yybackup:
    // Try to take a decision without lookahead.
    yyn = yypact_[+yystack_[0].state];
    if (yy_pact_value_is_default_ (yyn))
      goto yydefault;

    // Read a lookahead token.
    if (yyla.empty ())
      {
        YYCDEBUG << "Reading a token: ";
#if YY_EXCEPTIONS
        try
#endif // YY_EXCEPTIONS
          {
            yyla.type = yytranslate_ (yylex (&yyla.value));
          }
#if YY_EXCEPTIONS
        catch (const syntax_error& yyexc)
          {
            YYCDEBUG << "Caught exception: " << yyexc.what() << '\n';
            error (yyexc);
            goto yyerrlab1;
          }
#endif // YY_EXCEPTIONS
      }
    YY_SYMBOL_PRINT ("Next token is", yyla);

    /* If the proper action on seeing token YYLA.TYPE is to reduce or
       to detect an error, take that action.  */
    yyn += yyla.type_get ();
    if (yyn < 0 || yylast_ < yyn || yycheck_[yyn] != yyla.type_get ())
      {
        goto yydefault;
      }

    // Reduce or error.
    yyn = yytable_[yyn];
    if (yyn <= 0)
      {
        if (yy_table_value_is_error_ (yyn))
          goto yyerrlab;
        yyn = -yyn;
        goto yyreduce;
      }

    // Count tokens shifted since error; after three, turn off error status.
    if (yyerrstatus_)
      --yyerrstatus_;

    // Shift the lookahead token.
    yypush_ ("Shifting", state_type (yyn), YY_MOVE (yyla));
    goto yynewstate;


  /*-----------------------------------------------------------.
  | yydefault -- do the default action for the current state.  |
  `-----------------------------------------------------------*/
  yydefault:
    yyn = yydefact_[+yystack_[0].state];
    if (yyn == 0)
      goto yyerrlab;
    goto yyreduce;


  /*-----------------------------.
  | yyreduce -- do a reduction.  |
  `-----------------------------*/
  yyreduce:
    yylen = yyr2_[yyn];
    {
      stack_symbol_type yylhs;
      yylhs.state = yy_lr_goto_state_ (yystack_[yylen].state, yyr1_[yyn]);
      /* If YYLEN is nonzero, implement the default value of the
         action: '$$ = $1'.  Otherwise, use the top of the stack.

         Otherwise, the following line sets YYLHS.VALUE to garbage.
         This behavior is undocumented and Bison users should not rely
         upon it.  */
      if (yylen)
        yylhs.value = yystack_[yylen - 1].value;
      else
        yylhs.value = yystack_[0].value;


      // Perform the reduction.
      YY_REDUCE_PRINT (yyn);
#if YY_EXCEPTIONS
      try
#endif // YY_EXCEPTIONS
        {
          switch (yyn)
            {
  case 2:
#line 163 "holyc.yy"
{
	holyc::Token* eof_token = new holyc::Token(scanner.getLine(), scanner.getCol(), holyc::Parser::token::END);
	UnparseNode unparsed = *(yystack_[0].value.unparsePtr) + *eof_token;
	writeUnparsed(unparsed, std::cout);
	// std::cout << unparsed.str << std::endl;
}
#line 620 "parser.cc"
    break;

  case 3:
#line 171 "holyc.yy"
{
	(yylhs.value.unparsePtr) = new UnparseNode(*(yystack_[1].value.unparsePtr) + *(yystack_[0].value.unparsePtr));
}
#line 628 "parser.cc"
    break;

  case 4:
#line 175 "holyc.yy"
{
	(yylhs.value.unparsePtr) = new UnparseNode("");
}
#line 636 "parser.cc"
    break;

  case 5:
#line 180 "holyc.yy"
{
	(yylhs.value.unparsePtr) = new UnparseNode(*(yystack_[1].value.unparsePtr) + UnparseNode(*(yystack_[0].value.transToken)));
}
#line 644 "parser.cc"
    break;

  case 6:
#line 184 "holyc.yy"
{
	(yylhs.value.unparsePtr) = new UnparseNode(*(yystack_[0].value.unparsePtr));
}
#line 652 "parser.cc"
    break;

  case 7:
#line 189 "holyc.yy"
{
	(yylhs.value.unparsePtr) = new UnparseNode(*(yystack_[1].value.unparsePtr) + *(yystack_[0].value.unparsePtr));
}
#line 660 "parser.cc"
    break;

  case 8:
#line 194 "holyc.yy"
{
	(yylhs.value.unparsePtr) = new UnparseNode(*(yystack_[0].value.transToken));
}
#line 668 "parser.cc"
    break;

  case 9:
#line 198 "holyc.yy"
{
	(yylhs.value.unparsePtr) = new UnparseNode(*(yystack_[0].value.transToken));
}
#line 676 "parser.cc"
    break;

  case 10:
#line 202 "holyc.yy"
{
	(yylhs.value.unparsePtr) = new UnparseNode(*(yystack_[0].value.transToken));
}
#line 684 "parser.cc"
    break;

  case 11:
#line 206 "holyc.yy"
{
	(yylhs.value.unparsePtr) = new UnparseNode(*(yystack_[0].value.transToken));
}
#line 692 "parser.cc"
    break;

  case 12:
#line 210 "holyc.yy"
{
	(yylhs.value.unparsePtr) = new UnparseNode(*(yystack_[0].value.transToken));
}
#line 700 "parser.cc"
    break;

  case 13:
#line 214 "holyc.yy"
{
	(yylhs.value.unparsePtr) = new UnparseNode(*(yystack_[0].value.transToken));
}
#line 708 "parser.cc"
    break;

  case 14:
#line 218 "holyc.yy"
{
	(yylhs.value.unparsePtr) = new UnparseNode(*(yystack_[0].value.transToken));
}
#line 716 "parser.cc"
    break;

  case 15:
#line 223 "holyc.yy"
{
	(yylhs.value.unparsePtr) = new UnparseNode(*(yystack_[3].value.unparsePtr) + *(yystack_[2].value.unparsePtr) + *(yystack_[1].value.unparsePtr) + *(yystack_[0].value.unparsePtr));
}
#line 724 "parser.cc"
    break;

  case 16:
#line 229 "holyc.yy"
{
	(yylhs.value.unparsePtr) = new UnparseNode(UnparseNode(*(yystack_[1].value.transToken)) + UnparseNode(*(yystack_[0].value.transToken)));
}
#line 732 "parser.cc"
    break;

  case 17:
#line 234 "holyc.yy"
{
	(yylhs.value.unparsePtr) = new UnparseNode(UnparseNode(*(yystack_[2].value.transToken)) + *(yystack_[1].value.unparsePtr) + UnparseNode(*(yystack_[1].value.unparsePtr)));
}
#line 740 "parser.cc"
    break;

  case 18:
#line 240 "holyc.yy"
{
	(yylhs.value.unparsePtr) = new UnparseNode(*(yystack_[0].value.unparsePtr));
}
#line 748 "parser.cc"
    break;

  case 19:
#line 245 "holyc.yy"
{
	(yylhs.value.unparsePtr) = new UnparseNode(*(yystack_[2].value.unparsePtr) + UnparseNode(*(yystack_[1].value.transToken)) + *(yystack_[0].value.unparsePtr));
}
#line 756 "parser.cc"
    break;

  case 20:
#line 251 "holyc.yy"
{
	(yylhs.value.unparsePtr) = new UnparseNode(*(yystack_[1].value.unparsePtr) + *(yystack_[0].value.unparsePtr));
}
#line 764 "parser.cc"
    break;

  case 21:
#line 256 "holyc.yy"
{
	(yylhs.value.unparsePtr) = new UnparseNode(UnparseNode(*(yystack_[2].value.transToken)) + *(yystack_[1].value.unparsePtr) + UnparseNode(*(yystack_[0].value.transToken)));
}
#line 772 "parser.cc"
    break;

  case 22:
#line 261 "holyc.yy"
{
	(yylhs.value.unparsePtr) = new UnparseNode(*(yystack_[1].value.unparsePtr) + *(yystack_[0].value.unparsePtr));
}
#line 780 "parser.cc"
    break;

  case 23:
#line 266 "holyc.yy"
{
	(yylhs.value.unparsePtr) = new UnparseNode("");
}
#line 788 "parser.cc"
    break;

  case 24:
#line 272 "holyc.yy"
{
	(yylhs.value.unparsePtr) = new UnparseNode(*(yystack_[1].value.unparsePtr) + UnparseNode(*(yystack_[0].value.transToken)));
}
#line 796 "parser.cc"
    break;

  case 25:
#line 277 "holyc.yy"
{
	(yylhs.value.unparsePtr) = new UnparseNode(*(yystack_[1].value.unparsePtr) + UnparseNode(*(yystack_[0].value.transToken)));
}
#line 804 "parser.cc"
    break;

  case 26:
#line 282 "holyc.yy"
{
	(yylhs.value.unparsePtr) = new UnparseNode(*(yystack_[2].value.unparsePtr) + UnparseNode(*(yystack_[1].value.transToken)) + UnparseNode(*(yystack_[0].value.transToken)));
}
#line 812 "parser.cc"
    break;

  case 27:
#line 287 "holyc.yy"
{
	(yylhs.value.unparsePtr) = new UnparseNode(*(yystack_[2].value.unparsePtr) + UnparseNode(*(yystack_[1].value.transToken)) + UnparseNode(*(yystack_[0].value.transToken)));
}
#line 820 "parser.cc"
    break;

  case 28:
#line 292 "holyc.yy"
{
	(yylhs.value.unparsePtr) = new UnparseNode(UnparseNode(*(yystack_[2].value.transToken)) + *(yystack_[1].value.unparsePtr) + UnparseNode(*(yystack_[0].value.transToken)));
}
#line 828 "parser.cc"
    break;

  case 29:
#line 297 "holyc.yy"
{
	(yylhs.value.unparsePtr) = new UnparseNode(UnparseNode(*(yystack_[2].value.transToken)) + *(yystack_[1].value.unparsePtr) + UnparseNode(*(yystack_[0].value.transToken)));
}
#line 836 "parser.cc"
    break;

  case 30:
#line 302 "holyc.yy"
{
	(yylhs.value.unparsePtr) = new UnparseNode(UnparseNode(*(yystack_[6].value.transToken)) + UnparseNode(*(yystack_[5].value.transToken)) + *(yystack_[4].value.unparsePtr) + UnparseNode(*(yystack_[3].value.transToken)) + UnparseNode(*(yystack_[2].value.transToken)) + *(yystack_[1].value.unparsePtr) + UnparseNode(*(yystack_[0].value.transToken)));
}
#line 844 "parser.cc"
    break;

  case 31:
#line 307 "holyc.yy"
{
	(yylhs.value.unparsePtr) = new UnparseNode(UnparseNode(*(yystack_[10].value.transToken)) + UnparseNode(*(yystack_[9].value.transToken)) + *(yystack_[8].value.unparsePtr) + UnparseNode(*(yystack_[7].value.transToken)) + UnparseNode(*(yystack_[6].value.transToken)) + *(yystack_[5].value.unparsePtr) + UnparseNode(*(yystack_[4].value.transToken)) + UnparseNode(*(yystack_[3].value.transToken)) + UnparseNode(*(yystack_[2].value.transToken)) + *(yystack_[1].value.unparsePtr) + UnparseNode(*(yystack_[0].value.transToken)));
}
#line 852 "parser.cc"
    break;

  case 32:
#line 312 "holyc.yy"
{
	(yylhs.value.unparsePtr) = new UnparseNode(UnparseNode(*(yystack_[6].value.transToken)) + UnparseNode(*(yystack_[5].value.transToken)) + *(yystack_[4].value.unparsePtr) + UnparseNode(*(yystack_[3].value.transToken)) + UnparseNode(*(yystack_[2].value.transToken)) + *(yystack_[1].value.unparsePtr) + UnparseNode(*(yystack_[0].value.transToken)));
}
#line 860 "parser.cc"
    break;

  case 33:
#line 317 "holyc.yy"
{
	(yylhs.value.unparsePtr) = new UnparseNode(UnparseNode(*(yystack_[2].value.transToken)) + *(yystack_[1].value.unparsePtr) + UnparseNode(*(yystack_[0].value.transToken)));
}
#line 868 "parser.cc"
    break;

  case 34:
#line 322 "holyc.yy"
{
	(yylhs.value.unparsePtr) = new UnparseNode(UnparseNode(*(yystack_[1].value.transToken)) + UnparseNode(*(yystack_[0].value.transToken)));
}
#line 876 "parser.cc"
    break;

  case 35:
#line 327 "holyc.yy"
{
	(yylhs.value.unparsePtr) = new UnparseNode(*(yystack_[1].value.unparsePtr) + UnparseNode(*(yystack_[0].value.transToken)));
}
#line 884 "parser.cc"
    break;

  case 36:
#line 332 "holyc.yy"
{
	(yylhs.value.unparsePtr) = new UnparseNode(*(yystack_[2].value.unparsePtr) + UnparseNode(*(yystack_[1].value.transToken)) + UnparseNode(*(yystack_[0].value.transToken)));
}
#line 892 "parser.cc"
    break;

  case 37:
#line 337 "holyc.yy"
{
	(yylhs.value.unparsePtr) = new UnparseNode(*(yystack_[3].value.unparsePtr) + UnparseNode(*(yystack_[2].value.transToken)) + *(yystack_[1].value.unparsePtr) + UnparseNode(*(yystack_[0].value.transToken)));
}
#line 900 "parser.cc"
    break;

  case 38:
#line 343 "holyc.yy"
{
	(yylhs.value.unparsePtr) = (yystack_[0].value.unparsePtr);
}
#line 908 "parser.cc"
    break;

  case 39:
#line 348 "holyc.yy"
{
	(yylhs.value.unparsePtr) = new UnparseNode(*(yystack_[2].value.unparsePtr) + UnparseNode(*(yystack_[1].value.transToken)) + *(yystack_[0].value.unparsePtr));
}
#line 916 "parser.cc"
    break;

  case 40:
#line 354 "holyc.yy"
{
	(yylhs.value.unparsePtr) = new UnparseNode(UnparseNode(*(yystack_[1].value.transToken)) + *(yystack_[0].value.unparsePtr));
}
#line 924 "parser.cc"
    break;

  case 41:
#line 359 "holyc.yy"
{
	(yylhs.value.unparsePtr) = (yystack_[0].value.unparsePtr);
}
#line 932 "parser.cc"
    break;

  case 42:
#line 364 "holyc.yy"
{
	(yylhs.value.unparsePtr) = (yystack_[0].value.unparsePtr);
}
#line 940 "parser.cc"
    break;

  case 43:
#line 370 "holyc.yy"
{
	(yylhs.value.unparsePtr) = new UnparseNode(*(yystack_[2].value.unparsePtr) + UnparseNode(*(yystack_[1].value.transToken)) + *(yystack_[0].value.unparsePtr));
}
#line 948 "parser.cc"
    break;

  case 44:
#line 376 "holyc.yy"
{
	(yylhs.value.unparsePtr) = new UnparseNode(*(yystack_[2].value.unparsePtr) + UnparseNode(*(yystack_[1].value.transToken)) + *(yystack_[0].value.unparsePtr));
}
#line 956 "parser.cc"
    break;

  case 45:
#line 381 "holyc.yy"
{
	(yylhs.value.unparsePtr) = (yystack_[0].value.unparsePtr);
}
#line 964 "parser.cc"
    break;

  case 46:
#line 387 "holyc.yy"
{
	(yylhs.value.unparsePtr) = new UnparseNode(*(yystack_[2].value.unparsePtr) + UnparseNode(*(yystack_[1].value.transToken)) + *(yystack_[0].value.unparsePtr));
}
#line 972 "parser.cc"
    break;

  case 47:
#line 392 "holyc.yy"
{
	(yylhs.value.unparsePtr) = (yystack_[0].value.unparsePtr);
}
#line 980 "parser.cc"
    break;

  case 48:
#line 398 "holyc.yy"
{
	(yylhs.value.unparsePtr) = new UnparseNode(*(yystack_[2].value.unparsePtr) + *(yystack_[1].value.unparsePtr) + *(yystack_[0].value.unparsePtr));
}
#line 988 "parser.cc"
    break;

  case 49:
#line 403 "holyc.yy"
{
	(yylhs.value.unparsePtr) = (yystack_[0].value.unparsePtr);
}
#line 996 "parser.cc"
    break;

  case 50:
#line 409 "holyc.yy"
{
	(yylhs.value.unparsePtr) = new UnparseNode(*(yystack_[2].value.unparsePtr) + *(yystack_[1].value.unparsePtr) + *(yystack_[0].value.unparsePtr));
}
#line 1004 "parser.cc"
    break;

  case 51:
#line 414 "holyc.yy"
{
	(yylhs.value.unparsePtr) = (yystack_[0].value.unparsePtr);
}
#line 1012 "parser.cc"
    break;

  case 52:
#line 420 "holyc.yy"
{
	(yylhs.value.unparsePtr) = new UnparseNode(*(yystack_[2].value.unparsePtr) + *(yystack_[1].value.unparsePtr) + *(yystack_[0].value.unparsePtr));
}
#line 1020 "parser.cc"
    break;

  case 53:
#line 425 "holyc.yy"
{
	(yylhs.value.unparsePtr) = (yystack_[0].value.unparsePtr);
}
#line 1028 "parser.cc"
    break;

  case 54:
#line 431 "holyc.yy"
{
	(yylhs.value.unparsePtr) = new UnparseNode(UnparseNode(*(yystack_[1].value.transToken)) + *(yystack_[0].value.unparsePtr));
}
#line 1036 "parser.cc"
    break;

  case 55:
#line 436 "holyc.yy"
{
	(yylhs.value.unparsePtr) = (yystack_[0].value.unparsePtr);
}
#line 1044 "parser.cc"
    break;

  case 56:
#line 442 "holyc.yy"
{
	(yylhs.value.unparsePtr) = new UnparseNode(*(yystack_[0].value.transToken));
}
#line 1052 "parser.cc"
    break;

  case 57:
#line 447 "holyc.yy"
{
	(yylhs.value.unparsePtr) = new UnparseNode(*(yystack_[0].value.transToken));
}
#line 1060 "parser.cc"
    break;

  case 58:
#line 452 "holyc.yy"
{
	(yylhs.value.unparsePtr) = new UnparseNode(*(yystack_[0].value.transToken));
}
#line 1068 "parser.cc"
    break;

  case 59:
#line 457 "holyc.yy"
{
	(yylhs.value.unparsePtr) = new UnparseNode(*(yystack_[0].value.transToken));
}
#line 1076 "parser.cc"
    break;

  case 60:
#line 462 "holyc.yy"
{
	(yylhs.value.unparsePtr) = new UnparseNode(*(yystack_[0].value.transToken));
}
#line 1084 "parser.cc"
    break;

  case 61:
#line 467 "holyc.yy"
{
	(yylhs.value.unparsePtr) = new UnparseNode(*(yystack_[0].value.transToken));
}
#line 1092 "parser.cc"
    break;

  case 62:
#line 473 "holyc.yy"
{
	(yylhs.value.unparsePtr) = new UnparseNode(*(yystack_[0].value.transToken));
}
#line 1100 "parser.cc"
    break;

  case 63:
#line 478 "holyc.yy"
{
	(yylhs.value.unparsePtr) = new UnparseNode(*(yystack_[0].value.transToken));
}
#line 1108 "parser.cc"
    break;

  case 64:
#line 484 "holyc.yy"
{
	(yylhs.value.unparsePtr) = new UnparseNode(*(yystack_[0].value.transToken));
}
#line 1116 "parser.cc"
    break;

  case 65:
#line 489 "holyc.yy"
{
	(yylhs.value.unparsePtr) = new UnparseNode(*(yystack_[0].value.transToken));
}
#line 1124 "parser.cc"
    break;

  case 66:
#line 495 "holyc.yy"
{
	(yylhs.value.unparsePtr) = (yystack_[0].value.unparsePtr);
}
#line 1132 "parser.cc"
    break;

  case 67:
#line 500 "holyc.yy"
{
	(yylhs.value.unparsePtr) = new UnparseNode(*(yystack_[0].value.transIntToken));
}
#line 1140 "parser.cc"
    break;

  case 68:
#line 505 "holyc.yy"
{
	(yylhs.value.unparsePtr) = new UnparseNode(*(yystack_[0].value.transStrToken));
}
#line 1148 "parser.cc"
    break;

  case 69:
#line 510 "holyc.yy"
{
	(yylhs.value.unparsePtr) = new UnparseNode(*(yystack_[0].value.transCharToken));
}
#line 1156 "parser.cc"
    break;

  case 70:
#line 515 "holyc.yy"
{
	(yylhs.value.unparsePtr) = new UnparseNode(*(yystack_[0].value.transToken));
}
#line 1164 "parser.cc"
    break;

  case 71:
#line 520 "holyc.yy"
{
	(yylhs.value.unparsePtr) = new UnparseNode(*(yystack_[0].value.transToken));
}
#line 1172 "parser.cc"
    break;

  case 72:
#line 525 "holyc.yy"
{
	(yylhs.value.unparsePtr) = new UnparseNode(*(yystack_[0].value.transToken));
}
#line 1180 "parser.cc"
    break;

  case 73:
#line 530 "holyc.yy"
{
	(yylhs.value.unparsePtr) = new UnparseNode(UnparseNode(*(yystack_[2].value.transToken)) + *(yystack_[1].value.unparsePtr) + UnparseNode(*(yystack_[0].value.transToken)));
}
#line 1188 "parser.cc"
    break;

  case 74:
#line 535 "holyc.yy"
{
	(yylhs.value.unparsePtr) = (yystack_[0].value.unparsePtr);
}
#line 1196 "parser.cc"
    break;

  case 75:
#line 541 "holyc.yy"
{
	(yylhs.value.unparsePtr) = (yystack_[0].value.unparsePtr);
}
#line 1204 "parser.cc"
    break;

  case 76:
#line 546 "holyc.yy"
{
	(yylhs.value.unparsePtr) = new UnparseNode(*(yystack_[3].value.unparsePtr) + UnparseNode(*(yystack_[2].value.transToken)) + *(yystack_[1].value.unparsePtr) + UnparseNode(*(yystack_[0].value.transToken)));
}
#line 1212 "parser.cc"
    break;

  case 77:
#line 551 "holyc.yy"
{
	(yylhs.value.unparsePtr) = new UnparseNode(UnparseNode(*(yystack_[1].value.transToken)) + *(yystack_[0].value.unparsePtr));
}
#line 1220 "parser.cc"
    break;

  case 78:
#line 556 "holyc.yy"
{
	(yylhs.value.unparsePtr) = new UnparseNode(UnparseNode(*(yystack_[1].value.transToken)) + *(yystack_[0].value.unparsePtr));
}
#line 1228 "parser.cc"
    break;

  case 79:
#line 562 "holyc.yy"
{
	(yylhs.value.unparsePtr) = new UnparseNode(*(yystack_[0].value.transIDToken));
}
#line 1236 "parser.cc"
    break;


#line 1240 "parser.cc"

            default:
              break;
            }
        }
#if YY_EXCEPTIONS
      catch (const syntax_error& yyexc)
        {
          YYCDEBUG << "Caught exception: " << yyexc.what() << '\n';
          error (yyexc);
          YYERROR;
        }
#endif // YY_EXCEPTIONS
      YY_SYMBOL_PRINT ("-> $$ =", yylhs);
      yypop_ (yylen);
      yylen = 0;
      YY_STACK_PRINT ();

      // Shift the result of the reduction.
      yypush_ (YY_NULLPTR, YY_MOVE (yylhs));
    }
    goto yynewstate;


  /*--------------------------------------.
  | yyerrlab -- here on detecting error.  |
  `--------------------------------------*/
  yyerrlab:
    // If not already recovering from an error, report this error.
    if (!yyerrstatus_)
      {
        ++yynerrs_;
        error (yysyntax_error_ (yystack_[0].state, yyla));
      }


    if (yyerrstatus_ == 3)
      {
        /* If just tried and failed to reuse lookahead token after an
           error, discard it.  */

        // Return failure if at end of input.
        if (yyla.type_get () == yyeof_)
          YYABORT;
        else if (!yyla.empty ())
          {
            yy_destroy_ ("Error: discarding", yyla);
            yyla.clear ();
          }
      }

    // Else will try to reuse lookahead token after shifting the error token.
    goto yyerrlab1;


  /*---------------------------------------------------.
  | yyerrorlab -- error raised explicitly by YYERROR.  |
  `---------------------------------------------------*/
  yyerrorlab:
    /* Pacify compilers when the user code never invokes YYERROR and
       the label yyerrorlab therefore never appears in user code.  */
    if (false)
      YYERROR;

    /* Do not reclaim the symbols of the rule whose action triggered
       this YYERROR.  */
    yypop_ (yylen);
    yylen = 0;
    goto yyerrlab1;


  /*-------------------------------------------------------------.
  | yyerrlab1 -- common code for both syntax error and YYERROR.  |
  `-------------------------------------------------------------*/
  yyerrlab1:
    yyerrstatus_ = 3;   // Each real token shifted decrements this.
    {
      stack_symbol_type error_token;
      for (;;)
        {
          yyn = yypact_[+yystack_[0].state];
          if (!yy_pact_value_is_default_ (yyn))
            {
              yyn += yy_error_token_;
              if (0 <= yyn && yyn <= yylast_ && yycheck_[yyn] == yy_error_token_)
                {
                  yyn = yytable_[yyn];
                  if (0 < yyn)
                    break;
                }
            }

          // Pop the current state because it cannot handle the error token.
          if (yystack_.size () == 1)
            YYABORT;

          yy_destroy_ ("Error: popping", yystack_[0]);
          yypop_ ();
          YY_STACK_PRINT ();
        }


      // Shift the error token.
      error_token.state = state_type (yyn);
      yypush_ ("Shifting", YY_MOVE (error_token));
    }
    goto yynewstate;


  /*-------------------------------------.
  | yyacceptlab -- YYACCEPT comes here.  |
  `-------------------------------------*/
  yyacceptlab:
    yyresult = 0;
    goto yyreturn;


  /*-----------------------------------.
  | yyabortlab -- YYABORT comes here.  |
  `-----------------------------------*/
  yyabortlab:
    yyresult = 1;
    goto yyreturn;


  /*-----------------------------------------------------.
  | yyreturn -- parsing is finished, return the result.  |
  `-----------------------------------------------------*/
  yyreturn:
    if (!yyla.empty ())
      yy_destroy_ ("Cleanup: discarding lookahead", yyla);

    /* Do not reclaim the symbols of the rule whose action triggered
       this YYABORT or YYACCEPT.  */
    yypop_ (yylen);
    while (1 < yystack_.size ())
      {
        yy_destroy_ ("Cleanup: popping", yystack_[0]);
        yypop_ ();
      }

    return yyresult;
  }
#if YY_EXCEPTIONS
    catch (...)
      {
        YYCDEBUG << "Exception caught: cleaning lookahead and stack\n";
        // Do not try to display the values of the reclaimed symbols,
        // as their printers might throw an exception.
        if (!yyla.empty ())
          yy_destroy_ (YY_NULLPTR, yyla);

        while (1 < yystack_.size ())
          {
            yy_destroy_ (YY_NULLPTR, yystack_[0]);
            yypop_ ();
          }
        throw;
      }
#endif // YY_EXCEPTIONS
  }

  void
  Parser::error (const syntax_error& yyexc)
  {
    error (yyexc.what ());
  }

  // Generate an error message.
  std::string
  Parser::yysyntax_error_ (state_type yystate, const symbol_type& yyla) const
  {
    // Number of reported tokens (one for the "unexpected", one per
    // "expected").
    std::ptrdiff_t yycount = 0;
    // Its maximum.
    enum { YYERROR_VERBOSE_ARGS_MAXIMUM = 5 };
    // Arguments of yyformat.
    char const *yyarg[YYERROR_VERBOSE_ARGS_MAXIMUM];

    /* There are many possibilities here to consider:
       - If this state is a consistent state with a default action, then
         the only way this function was invoked is if the default action
         is an error action.  In that case, don't check for expected
         tokens because there are none.
       - The only way there can be no lookahead present (in yyla) is
         if this state is a consistent state with a default action.
         Thus, detecting the absence of a lookahead is sufficient to
         determine that there is no unexpected or expected token to
         report.  In that case, just report a simple "syntax error".
       - Don't assume there isn't a lookahead just because this state is
         a consistent state with a default action.  There might have
         been a previous inconsistent state, consistent state with a
         non-default action, or user semantic action that manipulated
         yyla.  (However, yyla is currently not documented for users.)
       - Of course, the expected token list depends on states to have
         correct lookahead information, and it depends on the parser not
         to perform extra reductions after fetching a lookahead from the
         scanner and before detecting a syntax error.  Thus, state merging
         (from LALR or IELR) and default reductions corrupt the expected
         token list.  However, the list is correct for canonical LR with
         one exception: it will still contain any token that will not be
         accepted due to an error action in a later state.
    */
    if (!yyla.empty ())
      {
        symbol_number_type yytoken = yyla.type_get ();
        yyarg[yycount++] = yytname_[yytoken];

        int yyn = yypact_[+yystate];
        if (!yy_pact_value_is_default_ (yyn))
          {
            /* Start YYX at -YYN if negative to avoid negative indexes in
               YYCHECK.  In other words, skip the first -YYN actions for
               this state because they are default actions.  */
            int yyxbegin = yyn < 0 ? -yyn : 0;
            // Stay within bounds of both yycheck and yytname.
            int yychecklim = yylast_ - yyn + 1;
            int yyxend = yychecklim < yyntokens_ ? yychecklim : yyntokens_;
            for (int yyx = yyxbegin; yyx < yyxend; ++yyx)
              if (yycheck_[yyx + yyn] == yyx && yyx != yy_error_token_
                  && !yy_table_value_is_error_ (yytable_[yyx + yyn]))
                {
                  if (yycount == YYERROR_VERBOSE_ARGS_MAXIMUM)
                    {
                      yycount = 1;
                      break;
                    }
                  else
                    yyarg[yycount++] = yytname_[yyx];
                }
          }
      }

    char const* yyformat = YY_NULLPTR;
    switch (yycount)
      {
#define YYCASE_(N, S)                         \
        case N:                               \
          yyformat = S;                       \
        break
      default: // Avoid compiler warnings.
        YYCASE_ (0, YY_("syntax error"));
        YYCASE_ (1, YY_("syntax error, unexpected %s"));
        YYCASE_ (2, YY_("syntax error, unexpected %s, expecting %s"));
        YYCASE_ (3, YY_("syntax error, unexpected %s, expecting %s or %s"));
        YYCASE_ (4, YY_("syntax error, unexpected %s, expecting %s or %s or %s"));
        YYCASE_ (5, YY_("syntax error, unexpected %s, expecting %s or %s or %s or %s"));
#undef YYCASE_
      }

    std::string yyres;
    // Argument number.
    std::ptrdiff_t yyi = 0;
    for (char const* yyp = yyformat; *yyp; ++yyp)
      if (yyp[0] == '%' && yyp[1] == 's' && yyi < yycount)
        {
          yyres += yytnamerr_ (yyarg[yyi++]);
          ++yyp;
        }
      else
        yyres += *yyp;
    return yyres;
  }


  const signed char Parser::yypact_ninf_ = -121;

  const signed char Parser::yytable_ninf_ = -1;

  const short
  Parser::yypact_[] =
  {
    -121,    17,   125,  -121,  -121,  -121,  -121,  -121,  -121,  -121,
    -121,  -121,   -34,     0,  -121,  -121,  -121,    -2,    44,     6,
    -121,     0,    -4,    35,  -121,  -121,  -121,  -121,   125,    18,
    -121,     0,     0,     7,    20,  -121,   171,   220,    22,     8,
       0,  -121,    19,    23,    54,   -16,  -121,  -121,    32,    30,
     220,  -121,   263,  -121,  -121,   220,   220,  -121,  -121,  -121,
    -121,  -121,    33,  -121,    12,    74,  -121,   287,   -10,  -121,
    -121,    77,    45,   220,  -121,  -121,  -121,  -121,   220,    47,
      48,   220,   210,  -121,    50,  -121,  -121,    56,  -121,  -121,
     253,   253,  -121,  -121,  -121,  -121,  -121,  -121,  -121,  -121,
     253,   253,  -121,  -121,   253,  -121,    58,  -121,  -121,  -121,
      66,  -121,    -9,  -121,    75,  -121,    74,  -121,    -5,   -10,
    -121,    83,  -121,   220,  -121,  -121,  -121,  -121,    72,   117,
      68,  -121,    84,  -121,   162,  -121
  };

  const signed char
  Parser::yydefact_[] =
  {
       4,     0,     2,     1,    10,    11,    12,    13,     8,     9,
      14,     3,     0,     0,     6,     5,    79,     7,     0,     0,
      16,     0,     0,    18,    23,    15,    20,    17,     0,     0,
      19,     0,     0,     0,     0,    21,     0,     0,     0,     0,
       0,    22,     0,     0,     0,    75,    77,    78,     0,    75,
       0,    69,     0,    71,    67,     0,     0,    72,    34,    68,
      70,    74,     0,    41,    42,    45,    47,    49,    51,    53,
      55,    66,     0,     0,    24,     7,    35,    25,     0,     0,
       0,     0,     0,    28,     0,    54,    66,     0,    40,    33,
       0,     0,    62,    63,    56,    58,    59,    60,    61,    57,
       0,     0,    65,    64,     0,    29,     0,    43,    27,    26,
       0,    36,     0,    38,     0,    73,    44,    46,    48,    50,
      52,     0,    76,     0,    37,    23,    23,    39,     0,     0,
      30,    32,     0,    23,     0,    31
  };

  const signed char
  Parser::yypgoto_[] =
  {
    -121,  -121,  -121,  -121,   104,    16,  -121,  -121,    87,  -121,
    -121,  -120,  -121,   -27,  -121,   -36,   -20,  -121,    26,    27,
      29,    21,    37,  -121,  -121,  -121,    78,   -29,   114
  };

  const signed char
  Parser::yydefgoto_[] =
  {
      -1,     1,     2,    11,    39,    40,    14,    19,    22,    23,
      25,    29,    41,    61,   112,    62,    63,    64,    65,    66,
      67,    68,    69,   100,   101,   104,    70,    71,    45
  };

  const unsigned char
  Parser::yytable_[] =
  {
      44,    72,    42,   123,    48,   128,   129,    15,    92,    43,
      93,    31,    81,   134,    84,    32,    82,     3,    13,    87,
      88,    16,    31,    86,     4,     5,    32,     6,    16,     7,
      18,   124,   102,   103,    21,    24,    27,   106,    33,    16,
      34,     8,   107,     9,    21,   110,   113,    28,    90,    74,
       4,     5,    50,     6,    73,     7,    35,    36,    81,    78,
      76,    86,    86,    37,    77,    10,    38,     8,    79,     9,
      80,    86,    86,    83,    89,    86,    31,    91,     4,     5,
      32,     6,    78,     7,    20,   132,   105,   127,   108,   109,
     114,    10,    33,    16,    34,     8,   115,     9,   121,    44,
      44,    42,    42,   122,   125,    44,    12,    42,    43,    43,
     130,    36,   126,   133,    43,    30,   116,    37,   117,    10,
      38,    31,   119,     4,     5,    32,     6,    17,     7,   118,
      85,     4,     5,     0,     6,    26,     7,    33,    16,    34,
       8,   120,     9,     0,     0,    46,    47,    49,     8,     0,
       9,     0,     0,     0,    75,   131,    36,     0,     0,     0,
       0,     0,    37,     0,    10,    38,    31,     0,     4,     5,
      32,     6,    10,     7,     0,    31,     0,     0,     0,    32,
       0,    51,    33,    16,    34,     8,    52,     9,     0,     0,
      53,     0,    16,     0,     0,    54,     0,     0,     0,     0,
     135,    36,     0,    55,    56,     0,    57,    37,     0,    10,
      38,     0,    58,     0,    31,    59,     0,    60,    32,     0,
      51,     0,     0,     0,    31,    52,     0,     0,    32,    53,
      51,    16,     0,     0,    54,    52,     0,     0,     0,    53,
       0,    16,    55,    56,    54,    57,     0,     0,     0,     0,
     111,     0,    55,    56,    59,    57,    60,    31,     0,     0,
       0,    32,     0,    51,    59,     0,    60,    31,    52,     0,
       0,    32,    53,    51,    16,     0,     0,    54,     0,     0,
       0,     0,    53,     0,    16,    55,     0,    54,    57,     0,
       0,     0,     0,     0,     0,    55,     0,    59,    57,    60,
      92,     0,    93,     0,     0,    94,     0,    59,     0,    60,
       0,     0,     0,    95,    96,     0,     0,    97,    98,     0,
       0,    99
  };

  const short
  Parser::yycheck_[] =
  {
      29,    37,    29,    12,    33,   125,   126,    41,    13,    29,
      15,     4,    28,   133,    50,     8,    32,     0,     2,    55,
      56,    21,     4,    52,     6,     7,     8,     9,    21,    11,
      32,    40,    42,    43,    18,    29,    40,    73,    20,    21,
      22,    23,    78,    25,    28,    81,    82,    12,    36,    41,
       6,     7,    32,     9,    32,    11,    38,    39,    28,     5,
      41,    90,    91,    45,    41,    47,    48,    23,    14,    25,
      16,   100,   101,    41,    41,   104,     4,     3,     6,     7,
       8,     9,     5,    11,    40,    17,    41,   123,    41,    41,
      40,    47,    20,    21,    22,    23,    40,    25,    40,   128,
     129,   128,   129,    37,    29,   134,     2,   134,   128,   129,
      38,    39,    29,    29,   134,    28,    90,    45,    91,    47,
      48,     4,   101,     6,     7,     8,     9,    13,    11,   100,
      52,     6,     7,    -1,     9,    21,    11,    20,    21,    22,
      23,   104,    25,    -1,    -1,    31,    32,    33,    23,    -1,
      25,    -1,    -1,    -1,    40,    38,    39,    -1,    -1,    -1,
      -1,    -1,    45,    -1,    47,    48,     4,    -1,     6,     7,
       8,     9,    47,    11,    -1,     4,    -1,    -1,    -1,     8,
      -1,    10,    20,    21,    22,    23,    15,    25,    -1,    -1,
      19,    -1,    21,    -1,    -1,    24,    -1,    -1,    -1,    -1,
      38,    39,    -1,    32,    33,    -1,    35,    45,    -1,    47,
      48,    -1,    41,    -1,     4,    44,    -1,    46,     8,    -1,
      10,    -1,    -1,    -1,     4,    15,    -1,    -1,     8,    19,
      10,    21,    -1,    -1,    24,    15,    -1,    -1,    -1,    19,
      -1,    21,    32,    33,    24,    35,    -1,    -1,    -1,    -1,
      40,    -1,    32,    33,    44,    35,    46,     4,    -1,    -1,
      -1,     8,    -1,    10,    44,    -1,    46,     4,    15,    -1,
      -1,     8,    19,    10,    21,    -1,    -1,    24,    -1,    -1,
      -1,    -1,    19,    -1,    21,    32,    -1,    24,    35,    -1,
      -1,    -1,    -1,    -1,    -1,    32,    -1,    44,    35,    46,
      13,    -1,    15,    -1,    -1,    18,    -1,    44,    -1,    46,
      -1,    -1,    -1,    26,    27,    -1,    -1,    30,    31,    -1,
      -1,    34
  };

  const signed char
  Parser::yystos_[] =
  {
       0,    50,    51,     0,     6,     7,     9,    11,    23,    25,
      47,    52,    53,    54,    55,    41,    21,    77,    32,    56,
      40,    54,    57,    58,    29,    59,    77,    40,    12,    60,
      57,     4,     8,    20,    22,    38,    39,    45,    48,    53,
      54,    61,    62,    65,    76,    77,    77,    77,    76,    77,
      32,    10,    15,    19,    24,    32,    33,    35,    41,    44,
      46,    62,    64,    65,    66,    67,    68,    69,    70,    71,
      75,    76,    64,    32,    41,    77,    41,    41,     5,    14,
      16,    28,    32,    41,    64,    75,    76,    64,    64,    41,
      36,     3,    13,    15,    18,    26,    27,    30,    31,    34,
      72,    73,    42,    43,    74,    41,    64,    64,    41,    41,
      64,    40,    63,    64,    40,    40,    67,    68,    69,    70,
      71,    40,    37,    12,    40,    29,    29,    64,    60,    60,
      38,    38,    17,    29,    60,    38
  };

  const signed char
  Parser::yyr1_[] =
  {
       0,    49,    50,    51,    51,    52,    52,    53,    54,    54,
      54,    54,    54,    54,    54,    55,    56,    56,    57,    57,
      58,    59,    60,    60,    61,    61,    61,    61,    61,    61,
      61,    61,    61,    61,    61,    61,    62,    62,    63,    63,
      64,    64,    64,    65,    66,    66,    67,    67,    68,    68,
      69,    69,    70,    70,    71,    71,    72,    72,    72,    72,
      72,    72,    73,    73,    74,    74,    75,    75,    75,    75,
      75,    75,    75,    75,    75,    76,    76,    76,    76,    77
  };

  const signed char
  Parser::yyr2_[] =
  {
       0,     2,     1,     2,     0,     2,     1,     2,     1,     1,
       1,     1,     1,     1,     1,     4,     2,     3,     1,     3,
       2,     3,     2,     0,     2,     2,     3,     3,     3,     3,
       7,    11,     7,     3,     2,     2,     3,     4,     1,     3,
       2,     1,     1,     3,     3,     1,     3,     1,     3,     1,
       3,     1,     3,     1,     2,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     3,     1,     1,     4,     2,     2,     1
  };



  // YYTNAME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
  // First, the terminals, then, starting at \a yyntokens_, nonterminals.
  const char*
  const Parser::yytname_[] =
  {
  "\"end file\"", "error", "$undefined", "AND", "AT", "ASSIGN", "BOOL",
  "BOOLPTR", "CARAT", "CHAR", "CHARLIT", "CHARPTR", "COMMA", "CROSS",
  "CROSSCROSS", "DASH", "DASHDASH", "ELSE", "EQUALS", "FALSE",
  "FROMCONSOLE", "ID", "IF", "INT", "INTLITERAL", "INTPTR", "GREATER",
  "GREATEREQ", "LBRACE", "LCURLY", "LESS", "LESSEQ", "LPAREN", "NOT",
  "NOTEQUALS", "NULLPTR", "OR", "RBRACE", "RCURLY", "RETURN", "RPAREN",
  "SEMICOLON", "SLASH", "STAR", "STRLITERAL", "TOCONSOLE", "TRUE", "VOID",
  "WHILE", "$accept", "program", "globals", "decl", "varDecl", "type",
  "fnDecl", "formals", "formalsList", "formalDecl", "fnBody", "stmtList",
  "stmt", "fncall", "actualsList", "exp", "assignExp", "orExp", "andExp",
  "cmpExp", "arithExp", "prodExp", "termExp", "cmpOp", "arithOp", "prodOp",
  "term", "lval", "id", YY_NULLPTR
  };

#if YYDEBUG
  const short
  Parser::yyrline_[] =
  {
       0,   162,   162,   170,   175,   179,   183,   188,   193,   197,
     201,   205,   209,   213,   217,   222,   228,   233,   239,   244,
     250,   255,   260,   266,   271,   276,   281,   286,   291,   296,
     301,   306,   311,   316,   321,   326,   331,   336,   342,   347,
     353,   358,   363,   369,   375,   380,   386,   391,   397,   402,
     408,   413,   419,   424,   430,   435,   441,   446,   451,   456,
     461,   466,   472,   477,   483,   488,   494,   499,   504,   509,
     514,   519,   524,   529,   534,   540,   545,   550,   555,   561
  };

  // Print the state stack on the debug stream.
  void
  Parser::yystack_print_ ()
  {
    *yycdebug_ << "Stack now";
    for (stack_type::const_iterator
           i = yystack_.begin (),
           i_end = yystack_.end ();
         i != i_end; ++i)
      *yycdebug_ << ' ' << int (i->state);
    *yycdebug_ << '\n';
  }

  // Report on the debug stream that the rule \a yyrule is going to be reduced.
  void
  Parser::yy_reduce_print_ (int yyrule)
  {
    int yylno = yyrline_[yyrule];
    int yynrhs = yyr2_[yyrule];
    // Print the symbols being reduced, and their result.
    *yycdebug_ << "Reducing stack by rule " << yyrule - 1
               << " (line " << yylno << "):\n";
    // The symbols being reduced.
    for (int yyi = 0; yyi < yynrhs; yyi++)
      YY_SYMBOL_PRINT ("   $" << yyi + 1 << " =",
                       yystack_[(yynrhs) - (yyi + 1)]);
  }
#endif // YYDEBUG

  Parser::token_number_type
  Parser::yytranslate_ (int t)
  {
    // YYTRANSLATE[TOKEN-NUM] -- Symbol number corresponding to
    // TOKEN-NUM as returned by yylex.
    static
    const token_number_type
    translate_table[] =
    {
       0,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     1,     2,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    19,    20,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48
    };
    const int user_token_number_max_ = 303;

    if (t <= 0)
      return yyeof_;
    else if (t <= user_token_number_max_)
      return translate_table[t];
    else
      return yy_undef_token_;
  }

#line 5 "holyc.yy"
} // holyc
#line 1803 "parser.cc"

#line 566 "holyc.yy"


void holyc::Parser::error(const std::string& err_message){
   /* For project grading, only report "syntax error"
      if a program has bad syntax. However, you will
      probably want better output for debugging. Thus,
      this error function prints a verbose message to 
      stdout, but only prints "syntax error" to stderr
   */
	std::cout << err_message << std::endl;
	std::cerr << "syntax error" << std::endl;
}
