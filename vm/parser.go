//line parser.y:5
package vm

import __yyfmt__ "fmt"

//line parser.y:5
import (
	"github.com/golang/glog"
	"github.com/google/mtail/metrics"
)

//line parser.y:14
type mtailSymType struct {
	yys      int
	intVal   int64
	floatVal float64
	op       int
	text     string
	texts    []string
	flag     bool
	n        astNode
	kind     metrics.Kind
}

const INVALID = 57346
const COUNTER = 57347
const GAUGE = 57348
const TIMER = 57349
const TEXT = 57350
const AS = 57351
const BY = 57352
const CONST = 57353
const HIDDEN = 57354
const DEF = 57355
const DEL = 57356
const NEXT = 57357
const OTHERWISE = 57358
const ELSE = 57359
const BUILTIN = 57360
const REGEX = 57361
const STRING = 57362
const CAPREF = 57363
const CAPREF_NAMED = 57364
const ID = 57365
const DECO = 57366
const INTLITERAL = 57367
const FLOATLITERAL = 57368
const INC = 57369
const DIV = 57370
const MOD = 57371
const MUL = 57372
const MINUS = 57373
const PLUS = 57374
const POW = 57375
const SHL = 57376
const SHR = 57377
const LT = 57378
const GT = 57379
const LE = 57380
const GE = 57381
const EQ = 57382
const NE = 57383
const BITAND = 57384
const XOR = 57385
const BITOR = 57386
const NOT = 57387
const AND = 57388
const OR = 57389
const ADD_ASSIGN = 57390
const ASSIGN = 57391
const CONCAT = 57392
const MATCH = 57393
const NOT_MATCH = 57394
const LCURLY = 57395
const RCURLY = 57396
const LPAREN = 57397
const RPAREN = 57398
const LSQUARE = 57399
const RSQUARE = 57400
const COMMA = 57401
const NL = 57402

var mtailToknames = [...]string{
	"$end",
	"error",
	"$unk",
	"INVALID",
	"COUNTER",
	"GAUGE",
	"TIMER",
	"TEXT",
	"AS",
	"BY",
	"CONST",
	"HIDDEN",
	"DEF",
	"DEL",
	"NEXT",
	"OTHERWISE",
	"ELSE",
	"BUILTIN",
	"REGEX",
	"STRING",
	"CAPREF",
	"CAPREF_NAMED",
	"ID",
	"DECO",
	"INTLITERAL",
	"FLOATLITERAL",
	"INC",
	"DIV",
	"MOD",
	"MUL",
	"MINUS",
	"PLUS",
	"POW",
	"SHL",
	"SHR",
	"LT",
	"GT",
	"LE",
	"GE",
	"EQ",
	"NE",
	"BITAND",
	"XOR",
	"BITOR",
	"NOT",
	"AND",
	"OR",
	"ADD_ASSIGN",
	"ASSIGN",
	"CONCAT",
	"MATCH",
	"NOT_MATCH",
	"LCURLY",
	"RCURLY",
	"LPAREN",
	"RPAREN",
	"LSQUARE",
	"RSQUARE",
	"COMMA",
	"NL",
}
var mtailStatenames = [...]string{}

const mtailEofCode = 1
const mtailErrCode = 2
const mtailInitialStackSize = 16

//line parser.y:573

//  tokenpos returns the position of the current token.
func tokenpos(mtaillex mtailLexer) position {
	return mtaillex.(*parser).t.pos
}

// markedpos returns the position recorded from the most recent mark_pos
// production.
func markedpos(mtaillex mtailLexer) position {
	return mtaillex.(*parser).pos
}

//line yacctab:1
var mtailExca = [...]int{
	-1, 1,
	1, -1,
	-2, 0,
	-1, 2,
	1, 1,
	13, 103,
	24, 103,
	28, 103,
	-2, 85,
	-1, 101,
	13, 103,
	24, 103,
	28, 103,
	-2, 85,
}

const mtailPrivate = 57344

const mtailLast = 222

var mtailAct = [...]int{

	19, 117, 46, 42, 26, 25, 41, 40, 24, 39,
	27, 47, 20, 115, 13, 86, 100, 44, 18, 23,
	144, 142, 143, 143, 82, 53, 52, 153, 120, 83,
	49, 50, 51, 74, 75, 2, 26, 25, 49, 81,
	88, 77, 76, 50, 51, 12, 63, 65, 64, 85,
	79, 80, 10, 22, 151, 11, 9, 14, 16, 30,
	28, 33, 31, 32, 43, 59, 35, 36, 99, 91,
	90, 87, 30, 107, 33, 31, 32, 43, 155, 35,
	36, 154, 43, 116, 116, 101, 38, 67, 68, 69,
	70, 71, 72, 84, 106, 126, 34, 119, 131, 38,
	124, 15, 25, 26, 25, 97, 94, 95, 93, 34,
	123, 96, 125, 136, 25, 25, 150, 18, 132, 135,
	134, 141, 140, 139, 146, 145, 137, 138, 133, 149,
	104, 108, 148, 103, 98, 109, 37, 105, 60, 130,
	129, 1, 110, 73, 152, 111, 112, 113, 45, 61,
	114, 92, 12, 59, 55, 56, 57, 58, 121, 10,
	22, 122, 11, 9, 14, 89, 30, 48, 33, 31,
	32, 43, 62, 35, 36, 30, 78, 33, 31, 32,
	43, 66, 35, 36, 30, 17, 33, 31, 32, 43,
	147, 35, 36, 38, 127, 128, 54, 8, 7, 102,
	6, 29, 38, 34, 21, 5, 4, 3, 15, 0,
	0, 0, 34, 118, 0, 0, 0, 0, 0, 0,
	0, 34,
}
var mtailPact = [...]int{

	-1000, -1000, 148, -1000, -1000, -1000, -1000, -1000, -1000, -1000,
	59, 166, -1000, -15, -23, -1000, -35, 149, 125, 4,
	-1000, -1000, -1000, 51, -1000, -18, -7, 16, 7, -33,
	-26, -1000, -1000, -1000, 54, -1000, -1000, 44, 54, 38,
	-1000, -1000, 78, -1000, -1000, 44, -1000, 117, -44, -1000,
	-1000, -1000, -1000, -1000, 110, -1000, -1000, -1000, -1000, -1000,
	71, -23, -44, -1000, -1000, -1000, -44, -1000, -1000, -1000,
	-1000, -1000, -1000, -44, -1000, -1000, -44, -44, -44, -1000,
	-1000, -44, 54, 157, -28, -3, 37, -1000, -1000, -44,
	-1000, -1000, -44, -1000, -1000, -1000, -1000, 7, -23, 54,
	-1000, 41, 130, -1000, -1000, 79, -23, -1000, 54, 54,
	166, 54, 54, 54, 59, -37, 4, -1000, -1000, -36,
	-1000, 54, 54, -1000, 4, -1000, -1000, -1000, -1000, 109,
	96, 26, -1000, 51, 16, -1000, -1000, -3, -3, 38,
	-1000, -1000, -1000, 54, -1000, 78, -1000, -32, -1000, -1000,
	-1000, -1000, 4, 58, -1000, -1000,
}
var mtailPgo = [...]int{

	0, 35, 207, 13, 11, 206, 205, 58, 2, 3,
	9, 136, 1, 204, 19, 10, 0, 14, 201, 6,
	60, 8, 200, 199, 198, 197, 7, 12, 196, 195,
	194, 190, 185, 181, 176, 172, 167, 165, 151, 143,
	141, 68, 15, 137,
}
var mtailR1 = [...]int{

	0, 40, 1, 1, 2, 2, 2, 2, 2, 2,
	2, 2, 2, 5, 5, 5, 6, 6, 4, 7,
	13, 13, 13, 17, 17, 17, 17, 36, 36, 16,
	16, 35, 35, 35, 14, 14, 33, 33, 33, 33,
	33, 33, 15, 15, 34, 34, 10, 10, 27, 27,
	27, 39, 39, 21, 20, 20, 20, 37, 37, 9,
	9, 38, 38, 38, 38, 12, 12, 11, 11, 8,
	8, 8, 8, 8, 8, 8, 8, 8, 18, 18,
	19, 3, 3, 26, 22, 32, 32, 23, 23, 23,
	23, 28, 28, 28, 28, 30, 31, 31, 31, 31,
	29, 24, 25, 42, 43, 41, 41,
}
var mtailR2 = [...]int{

	0, 1, 0, 2, 1, 1, 1, 1, 1, 1,
	3, 2, 1, 4, 2, 2, 1, 2, 3, 1,
	1, 4, 4, 1, 1, 4, 4, 1, 1, 1,
	4, 1, 1, 1, 1, 4, 1, 1, 1, 1,
	1, 1, 1, 4, 1, 1, 1, 4, 1, 4,
	4, 1, 1, 1, 1, 4, 4, 1, 1, 1,
	4, 1, 1, 1, 1, 1, 2, 1, 2, 1,
	3, 4, 1, 1, 1, 3, 1, 1, 1, 4,
	1, 1, 3, 5, 3, 0, 1, 2, 2, 1,
	1, 1, 1, 1, 1, 2, 1, 1, 3, 3,
	2, 4, 3, 0, 0, 0, 1,
}
var mtailChk = [...]int{

	-1000, -40, -1, -2, -5, -6, -22, -24, -25, 15,
	11, 14, 4, -17, 16, 60, -7, -32, -42, -16,
	-27, -13, 12, -14, -21, -8, -12, -15, -20, -18,
	18, 21, 22, 20, 55, 25, 26, -11, 45, -10,
	-26, -19, -9, 23, -19, -11, -8, -4, -36, 53,
	46, 47, -4, 60, -28, 5, 6, 7, 8, 28,
	13, 24, -35, 42, 44, 43, -33, 36, 37, 38,
	39, 40, 41, -39, 51, 52, 49, 48, -34, 34,
	35, 32, 57, 55, -7, -17, -42, 27, -12, -37,
	32, 31, -38, 30, 28, 29, 33, -20, 17, -41,
	60, -1, -23, 23, 20, -43, 23, -4, -41, -41,
	-41, -41, -41, -41, -41, -3, -16, -12, 56, -3,
	56, -41, -41, -4, -16, -27, 54, -30, -29, 10,
	9, 19, -4, -14, -15, -21, -8, -17, -17, -10,
	-26, -19, 58, 59, 56, -9, -12, -31, 23, 20,
	20, 28, -16, 59, 23, 20,
}
var mtailDef = [...]int{

	2, -2, -2, 3, 4, 5, 6, 7, 8, 9,
	0, 0, 12, 20, 0, 16, 0, 0, 0, 23,
	24, 19, 86, 29, 48, 67, 59, 34, 53, 69,
	0, 72, 73, 74, 103, 76, 77, 65, 0, 42,
	54, 78, 46, 80, 103, 11, 67, 14, 105, 2,
	27, 28, 15, 17, 0, 91, 92, 93, 94, 104,
	0, 0, 105, 31, 32, 33, 105, 36, 37, 38,
	39, 40, 41, 105, 51, 52, 105, 105, 105, 44,
	45, 105, 0, 0, 0, 20, 0, 68, 66, 105,
	57, 58, 105, 61, 62, 63, 64, 10, 0, 103,
	106, -2, 84, 89, 90, 0, 0, 102, 0, 0,
	103, 103, 103, 0, 103, 0, 81, 59, 70, 0,
	75, 0, 0, 13, 25, 26, 18, 87, 88, 0,
	0, 0, 101, 30, 35, 49, 50, 21, 22, 43,
	55, 56, 79, 0, 71, 47, 60, 95, 96, 97,
	100, 83, 82, 0, 98, 99,
}
var mtailTok1 = [...]int{

	1,
}
var mtailTok2 = [...]int{

	2, 3, 4, 5, 6, 7, 8, 9, 10, 11,
	12, 13, 14, 15, 16, 17, 18, 19, 20, 21,
	22, 23, 24, 25, 26, 27, 28, 29, 30, 31,
	32, 33, 34, 35, 36, 37, 38, 39, 40, 41,
	42, 43, 44, 45, 46, 47, 48, 49, 50, 51,
	52, 53, 54, 55, 56, 57, 58, 59, 60,
}
var mtailTok3 = [...]int{
	0,
}

var mtailErrorMessages = [...]struct {
	state int
	token int
	msg   string
}{
	{105, 4, "unexpected end of file"},
}

//line yaccpar:1

/*	parser for yacc output	*/

var (
	mtailDebug        = 0
	mtailErrorVerbose = false
)

type mtailLexer interface {
	Lex(lval *mtailSymType) int
	Error(s string)
}

type mtailParser interface {
	Parse(mtailLexer) int
	Lookahead() int
}

type mtailParserImpl struct {
	lval  mtailSymType
	stack [mtailInitialStackSize]mtailSymType
	char  int
}

func (p *mtailParserImpl) Lookahead() int {
	return p.char
}

func mtailNewParser() mtailParser {
	return &mtailParserImpl{}
}

const mtailFlag = -1000

func mtailTokname(c int) string {
	if c >= 1 && c-1 < len(mtailToknames) {
		if mtailToknames[c-1] != "" {
			return mtailToknames[c-1]
		}
	}
	return __yyfmt__.Sprintf("tok-%v", c)
}

func mtailStatname(s int) string {
	if s >= 0 && s < len(mtailStatenames) {
		if mtailStatenames[s] != "" {
			return mtailStatenames[s]
		}
	}
	return __yyfmt__.Sprintf("state-%v", s)
}

func mtailErrorMessage(state, lookAhead int) string {
	const TOKSTART = 4

	if !mtailErrorVerbose {
		return "syntax error"
	}

	for _, e := range mtailErrorMessages {
		if e.state == state && e.token == lookAhead {
			return "syntax error: " + e.msg
		}
	}

	res := "syntax error: unexpected " + mtailTokname(lookAhead)

	// To match Bison, suggest at most four expected tokens.
	expected := make([]int, 0, 4)

	// Look for shiftable tokens.
	base := mtailPact[state]
	for tok := TOKSTART; tok-1 < len(mtailToknames); tok++ {
		if n := base + tok; n >= 0 && n < mtailLast && mtailChk[mtailAct[n]] == tok {
			if len(expected) == cap(expected) {
				return res
			}
			expected = append(expected, tok)
		}
	}

	if mtailDef[state] == -2 {
		i := 0
		for mtailExca[i] != -1 || mtailExca[i+1] != state {
			i += 2
		}

		// Look for tokens that we accept or reduce.
		for i += 2; mtailExca[i] >= 0; i += 2 {
			tok := mtailExca[i]
			if tok < TOKSTART || mtailExca[i+1] == 0 {
				continue
			}
			if len(expected) == cap(expected) {
				return res
			}
			expected = append(expected, tok)
		}

		// If the default action is to accept or reduce, give up.
		if mtailExca[i+1] != 0 {
			return res
		}
	}

	for i, tok := range expected {
		if i == 0 {
			res += ", expecting "
		} else {
			res += " or "
		}
		res += mtailTokname(tok)
	}
	return res
}

func mtaillex1(lex mtailLexer, lval *mtailSymType) (char, token int) {
	token = 0
	char = lex.Lex(lval)
	if char <= 0 {
		token = mtailTok1[0]
		goto out
	}
	if char < len(mtailTok1) {
		token = mtailTok1[char]
		goto out
	}
	if char >= mtailPrivate {
		if char < mtailPrivate+len(mtailTok2) {
			token = mtailTok2[char-mtailPrivate]
			goto out
		}
	}
	for i := 0; i < len(mtailTok3); i += 2 {
		token = mtailTok3[i+0]
		if token == char {
			token = mtailTok3[i+1]
			goto out
		}
	}

out:
	if token == 0 {
		token = mtailTok2[1] /* unknown char */
	}
	if mtailDebug >= 3 {
		__yyfmt__.Printf("lex %s(%d)\n", mtailTokname(token), uint(char))
	}
	return char, token
}

func mtailParse(mtaillex mtailLexer) int {
	return mtailNewParser().Parse(mtaillex)
}

func (mtailrcvr *mtailParserImpl) Parse(mtaillex mtailLexer) int {
	var mtailn int
	var mtailVAL mtailSymType
	var mtailDollar []mtailSymType
	_ = mtailDollar // silence set and not used
	mtailS := mtailrcvr.stack[:]

	Nerrs := 0   /* number of errors */
	Errflag := 0 /* error recovery flag */
	mtailstate := 0
	mtailrcvr.char = -1
	mtailtoken := -1 // mtailrcvr.char translated into internal numbering
	defer func() {
		// Make sure we report no lookahead when not parsing.
		mtailstate = -1
		mtailrcvr.char = -1
		mtailtoken = -1
	}()
	mtailp := -1
	goto mtailstack

ret0:
	return 0

ret1:
	return 1

mtailstack:
	/* put a state and value onto the stack */
	if mtailDebug >= 4 {
		__yyfmt__.Printf("char %v in %v\n", mtailTokname(mtailtoken), mtailStatname(mtailstate))
	}

	mtailp++
	if mtailp >= len(mtailS) {
		nyys := make([]mtailSymType, len(mtailS)*2)
		copy(nyys, mtailS)
		mtailS = nyys
	}
	mtailS[mtailp] = mtailVAL
	mtailS[mtailp].yys = mtailstate

mtailnewstate:
	mtailn = mtailPact[mtailstate]
	if mtailn <= mtailFlag {
		goto mtaildefault /* simple state */
	}
	if mtailrcvr.char < 0 {
		mtailrcvr.char, mtailtoken = mtaillex1(mtaillex, &mtailrcvr.lval)
	}
	mtailn += mtailtoken
	if mtailn < 0 || mtailn >= mtailLast {
		goto mtaildefault
	}
	mtailn = mtailAct[mtailn]
	if mtailChk[mtailn] == mtailtoken { /* valid shift */
		mtailrcvr.char = -1
		mtailtoken = -1
		mtailVAL = mtailrcvr.lval
		mtailstate = mtailn
		if Errflag > 0 {
			Errflag--
		}
		goto mtailstack
	}

mtaildefault:
	/* default state action */
	mtailn = mtailDef[mtailstate]
	if mtailn == -2 {
		if mtailrcvr.char < 0 {
			mtailrcvr.char, mtailtoken = mtaillex1(mtaillex, &mtailrcvr.lval)
		}

		/* look through exception table */
		xi := 0
		for {
			if mtailExca[xi+0] == -1 && mtailExca[xi+1] == mtailstate {
				break
			}
			xi += 2
		}
		for xi += 2; ; xi += 2 {
			mtailn = mtailExca[xi+0]
			if mtailn < 0 || mtailn == mtailtoken {
				break
			}
		}
		mtailn = mtailExca[xi+1]
		if mtailn < 0 {
			goto ret0
		}
	}
	if mtailn == 0 {
		/* error ... attempt to resume parsing */
		switch Errflag {
		case 0: /* brand new error */
			mtaillex.Error(mtailErrorMessage(mtailstate, mtailtoken))
			Nerrs++
			if mtailDebug >= 1 {
				__yyfmt__.Printf("%s", mtailStatname(mtailstate))
				__yyfmt__.Printf(" saw %s\n", mtailTokname(mtailtoken))
			}
			fallthrough

		case 1, 2: /* incompletely recovered error ... try again */
			Errflag = 3

			/* find a state where "error" is a legal shift action */
			for mtailp >= 0 {
				mtailn = mtailPact[mtailS[mtailp].yys] + mtailErrCode
				if mtailn >= 0 && mtailn < mtailLast {
					mtailstate = mtailAct[mtailn] /* simulate a shift of "error" */
					if mtailChk[mtailstate] == mtailErrCode {
						goto mtailstack
					}
				}

				/* the current p has no shift on "error", pop stack */
				if mtailDebug >= 2 {
					__yyfmt__.Printf("error recovery pops state %d\n", mtailS[mtailp].yys)
				}
				mtailp--
			}
			/* there is no state on the stack with an error shift ... abort */
			goto ret1

		case 3: /* no shift yet; clobber input char */
			if mtailDebug >= 2 {
				__yyfmt__.Printf("error recovery discards %s\n", mtailTokname(mtailtoken))
			}
			if mtailtoken == mtailEofCode {
				goto ret1
			}
			mtailrcvr.char = -1
			mtailtoken = -1
			goto mtailnewstate /* try again in the same state */
		}
	}

	/* reduction by production mtailn */
	if mtailDebug >= 2 {
		__yyfmt__.Printf("reduce %v in:\n\t%v\n", mtailn, mtailStatname(mtailstate))
	}

	mtailnt := mtailn
	mtailpt := mtailp
	_ = mtailpt // guard against "declared and not used"

	mtailp -= mtailR2[mtailn]
	// mtailp is now the index of $0. Perform the default action. Iff the
	// reduced production is ε, $1 is possibly out of range.
	if mtailp+1 >= len(mtailS) {
		nyys := make([]mtailSymType, len(mtailS)*2)
		copy(nyys, mtailS)
		mtailS = nyys
	}
	mtailVAL = mtailS[mtailp+1]

	/* consult goto table to find next state */
	mtailn = mtailR1[mtailn]
	mtailg := mtailPgo[mtailn]
	mtailj := mtailg + mtailS[mtailp].yys + 1

	if mtailj >= mtailLast {
		mtailstate = mtailAct[mtailg]
	} else {
		mtailstate = mtailAct[mtailj]
		if mtailChk[mtailstate] != -mtailn {
			mtailstate = mtailAct[mtailg]
		}
	}
	// dummy call; replaced with literal code
	switch mtailnt {

	case 1:
		mtailDollar = mtailS[mtailpt-1 : mtailpt+1]
		//line parser.y:76
		{
			mtaillex.(*parser).root = mtailDollar[1].n
		}
	case 2:
		mtailDollar = mtailS[mtailpt-0 : mtailpt+1]
		//line parser.y:83
		{
			mtailVAL.n = &stmtlistNode{}
		}
	case 3:
		mtailDollar = mtailS[mtailpt-2 : mtailpt+1]
		//line parser.y:87
		{
			mtailVAL.n = mtailDollar[1].n
			if mtailDollar[2].n != nil {
				mtailVAL.n.(*stmtlistNode).children = append(mtailVAL.n.(*stmtlistNode).children, mtailDollar[2].n)
			}
		}
	case 4:
		mtailDollar = mtailS[mtailpt-1 : mtailpt+1]
		//line parser.y:97
		{
			mtailVAL.n = mtailDollar[1].n
		}
	case 5:
		mtailDollar = mtailS[mtailpt-1 : mtailpt+1]
		//line parser.y:99
		{
			mtailVAL.n = mtailDollar[1].n
		}
	case 6:
		mtailDollar = mtailS[mtailpt-1 : mtailpt+1]
		//line parser.y:101
		{
			mtailVAL.n = mtailDollar[1].n
		}
	case 7:
		mtailDollar = mtailS[mtailpt-1 : mtailpt+1]
		//line parser.y:103
		{
			mtailVAL.n = mtailDollar[1].n
		}
	case 8:
		mtailDollar = mtailS[mtailpt-1 : mtailpt+1]
		//line parser.y:105
		{
			mtailVAL.n = mtailDollar[1].n
		}
	case 9:
		mtailDollar = mtailS[mtailpt-1 : mtailpt+1]
		//line parser.y:107
		{
			mtailVAL.n = &nextNode{tokenpos(mtaillex)}
		}
	case 10:
		mtailDollar = mtailS[mtailpt-3 : mtailpt+1]
		//line parser.y:111
		{
			mtailVAL.n = &patternFragmentDefNode{id: mtailDollar[2].n, expr: mtailDollar[3].n}
		}
	case 11:
		mtailDollar = mtailS[mtailpt-2 : mtailpt+1]
		//line parser.y:115
		{
			mtailVAL.n = &delNode{tokenpos(mtaillex), mtailDollar[2].n}
		}
	case 12:
		mtailDollar = mtailS[mtailpt-1 : mtailpt+1]
		//line parser.y:119
		{
			mtailVAL.n = &errorNode{tokenpos(mtaillex), mtailDollar[1].text}
		}
	case 13:
		mtailDollar = mtailS[mtailpt-4 : mtailpt+1]
		//line parser.y:126
		{
			mtailVAL.n = &condNode{mtailDollar[1].n, mtailDollar[2].n, mtailDollar[4].n, nil}
		}
	case 14:
		mtailDollar = mtailS[mtailpt-2 : mtailpt+1]
		//line parser.y:130
		{
			if mtailDollar[1].n != nil {
				mtailVAL.n = &condNode{mtailDollar[1].n, mtailDollar[2].n, nil, nil}
			} else {
				mtailVAL.n = mtailDollar[2].n
			}
		}
	case 15:
		mtailDollar = mtailS[mtailpt-2 : mtailpt+1]
		//line parser.y:138
		{
			o := &otherwiseNode{tokenpos(mtaillex)}
			mtailVAL.n = &condNode{o, mtailDollar[2].n, nil, nil}
		}
	case 16:
		mtailDollar = mtailS[mtailpt-1 : mtailpt+1]
		//line parser.y:146
		{
			mtailVAL.n = nil
		}
	case 17:
		mtailDollar = mtailS[mtailpt-2 : mtailpt+1]
		//line parser.y:148
		{
			mtailVAL.n = mtailDollar[1].n
		}
	case 18:
		mtailDollar = mtailS[mtailpt-3 : mtailpt+1]
		//line parser.y:153
		{
			mtailVAL.n = mtailDollar[2].n
		}
	case 19:
		mtailDollar = mtailS[mtailpt-1 : mtailpt+1]
		//line parser.y:160
		{
			mtailVAL.n = mtailDollar[1].n
		}
	case 20:
		mtailDollar = mtailS[mtailpt-1 : mtailpt+1]
		//line parser.y:165
		{
			mtailVAL.n = mtailDollar[1].n
		}
	case 21:
		mtailDollar = mtailS[mtailpt-4 : mtailpt+1]
		//line parser.y:169
		{
			mtailVAL.n = &binaryExprNode{lhs: mtailDollar[1].n, rhs: mtailDollar[4].n, op: mtailDollar[2].op}
		}
	case 22:
		mtailDollar = mtailS[mtailpt-4 : mtailpt+1]
		//line parser.y:173
		{
			mtailVAL.n = &binaryExprNode{lhs: mtailDollar[1].n, rhs: mtailDollar[4].n, op: mtailDollar[2].op}
		}
	case 23:
		mtailDollar = mtailS[mtailpt-1 : mtailpt+1]
		//line parser.y:180
		{
			mtailVAL.n = mtailDollar[1].n
		}
	case 24:
		mtailDollar = mtailS[mtailpt-1 : mtailpt+1]
		//line parser.y:182
		{
			mtailVAL.n = mtailDollar[1].n
		}
	case 25:
		mtailDollar = mtailS[mtailpt-4 : mtailpt+1]
		//line parser.y:184
		{
			mtailVAL.n = &binaryExprNode{lhs: mtailDollar[1].n, rhs: mtailDollar[4].n, op: mtailDollar[2].op}
		}
	case 26:
		mtailDollar = mtailS[mtailpt-4 : mtailpt+1]
		//line parser.y:188
		{
			mtailVAL.n = &binaryExprNode{lhs: mtailDollar[1].n, rhs: mtailDollar[4].n, op: mtailDollar[2].op}
		}
	case 27:
		mtailDollar = mtailS[mtailpt-1 : mtailpt+1]
		//line parser.y:195
		{
			mtailVAL.op = mtailDollar[1].op
		}
	case 28:
		mtailDollar = mtailS[mtailpt-1 : mtailpt+1]
		//line parser.y:197
		{
			mtailVAL.op = mtailDollar[1].op
		}
	case 29:
		mtailDollar = mtailS[mtailpt-1 : mtailpt+1]
		//line parser.y:202
		{
			mtailVAL.n = mtailDollar[1].n
		}
	case 30:
		mtailDollar = mtailS[mtailpt-4 : mtailpt+1]
		//line parser.y:204
		{
			mtailVAL.n = &binaryExprNode{lhs: mtailDollar[1].n, rhs: mtailDollar[4].n, op: mtailDollar[2].op}
		}
	case 31:
		mtailDollar = mtailS[mtailpt-1 : mtailpt+1]
		//line parser.y:211
		{
			mtailVAL.op = mtailDollar[1].op
		}
	case 32:
		mtailDollar = mtailS[mtailpt-1 : mtailpt+1]
		//line parser.y:213
		{
			mtailVAL.op = mtailDollar[1].op
		}
	case 33:
		mtailDollar = mtailS[mtailpt-1 : mtailpt+1]
		//line parser.y:215
		{
			mtailVAL.op = mtailDollar[1].op
		}
	case 34:
		mtailDollar = mtailS[mtailpt-1 : mtailpt+1]
		//line parser.y:220
		{
			mtailVAL.n = mtailDollar[1].n
		}
	case 35:
		mtailDollar = mtailS[mtailpt-4 : mtailpt+1]
		//line parser.y:222
		{
			mtailVAL.n = &binaryExprNode{lhs: mtailDollar[1].n, rhs: mtailDollar[4].n, op: mtailDollar[2].op}
		}
	case 36:
		mtailDollar = mtailS[mtailpt-1 : mtailpt+1]
		//line parser.y:229
		{
			mtailVAL.op = mtailDollar[1].op
		}
	case 37:
		mtailDollar = mtailS[mtailpt-1 : mtailpt+1]
		//line parser.y:231
		{
			mtailVAL.op = mtailDollar[1].op
		}
	case 38:
		mtailDollar = mtailS[mtailpt-1 : mtailpt+1]
		//line parser.y:233
		{
			mtailVAL.op = mtailDollar[1].op
		}
	case 39:
		mtailDollar = mtailS[mtailpt-1 : mtailpt+1]
		//line parser.y:235
		{
			mtailVAL.op = mtailDollar[1].op
		}
	case 40:
		mtailDollar = mtailS[mtailpt-1 : mtailpt+1]
		//line parser.y:237
		{
			mtailVAL.op = mtailDollar[1].op
		}
	case 41:
		mtailDollar = mtailS[mtailpt-1 : mtailpt+1]
		//line parser.y:239
		{
			mtailVAL.op = mtailDollar[1].op
		}
	case 42:
		mtailDollar = mtailS[mtailpt-1 : mtailpt+1]
		//line parser.y:244
		{
			mtailVAL.n = mtailDollar[1].n
		}
	case 43:
		mtailDollar = mtailS[mtailpt-4 : mtailpt+1]
		//line parser.y:246
		{
			mtailVAL.n = &binaryExprNode{lhs: mtailDollar[1].n, rhs: mtailDollar[4].n, op: mtailDollar[2].op}
		}
	case 44:
		mtailDollar = mtailS[mtailpt-1 : mtailpt+1]
		//line parser.y:253
		{
			mtailVAL.op = mtailDollar[1].op
		}
	case 45:
		mtailDollar = mtailS[mtailpt-1 : mtailpt+1]
		//line parser.y:255
		{
			mtailVAL.op = mtailDollar[1].op
		}
	case 46:
		mtailDollar = mtailS[mtailpt-1 : mtailpt+1]
		//line parser.y:260
		{
			mtailVAL.n = mtailDollar[1].n
		}
	case 47:
		mtailDollar = mtailS[mtailpt-4 : mtailpt+1]
		//line parser.y:262
		{
			mtailVAL.n = &binaryExprNode{lhs: mtailDollar[1].n, rhs: mtailDollar[4].n, op: mtailDollar[2].op}
		}
	case 48:
		mtailDollar = mtailS[mtailpt-1 : mtailpt+1]
		//line parser.y:269
		{
			mtailVAL.n = mtailDollar[1].n
		}
	case 49:
		mtailDollar = mtailS[mtailpt-4 : mtailpt+1]
		//line parser.y:271
		{
			mtailVAL.n = &binaryExprNode{lhs: mtailDollar[1].n, rhs: mtailDollar[4].n, op: mtailDollar[2].op}
		}
	case 50:
		mtailDollar = mtailS[mtailpt-4 : mtailpt+1]
		//line parser.y:275
		{
			mtailVAL.n = &binaryExprNode{lhs: mtailDollar[1].n, rhs: mtailDollar[4].n, op: mtailDollar[2].op}
		}
	case 51:
		mtailDollar = mtailS[mtailpt-1 : mtailpt+1]
		//line parser.y:282
		{
			mtailVAL.op = mtailDollar[1].op
		}
	case 52:
		mtailDollar = mtailS[mtailpt-1 : mtailpt+1]
		//line parser.y:284
		{
			mtailVAL.op = mtailDollar[1].op
		}
	case 53:
		mtailDollar = mtailS[mtailpt-1 : mtailpt+1]
		//line parser.y:289
		{
			mtailVAL.n = &patternExprNode{expr: mtailDollar[1].n}
		}
	case 54:
		mtailDollar = mtailS[mtailpt-1 : mtailpt+1]
		//line parser.y:296
		{
			mtailVAL.n = mtailDollar[1].n
		}
	case 55:
		mtailDollar = mtailS[mtailpt-4 : mtailpt+1]
		//line parser.y:298
		{
			mtailVAL.n = &binaryExprNode{lhs: mtailDollar[1].n, rhs: mtailDollar[4].n, op: CONCAT}
		}
	case 56:
		mtailDollar = mtailS[mtailpt-4 : mtailpt+1]
		//line parser.y:302
		{
			mtailVAL.n = &binaryExprNode{lhs: mtailDollar[1].n, rhs: mtailDollar[4].n, op: CONCAT}
		}
	case 57:
		mtailDollar = mtailS[mtailpt-1 : mtailpt+1]
		//line parser.y:309
		{
			mtailVAL.op = mtailDollar[1].op
		}
	case 58:
		mtailDollar = mtailS[mtailpt-1 : mtailpt+1]
		//line parser.y:311
		{
			mtailVAL.op = mtailDollar[1].op
		}
	case 59:
		mtailDollar = mtailS[mtailpt-1 : mtailpt+1]
		//line parser.y:316
		{
			mtailVAL.n = mtailDollar[1].n
		}
	case 60:
		mtailDollar = mtailS[mtailpt-4 : mtailpt+1]
		//line parser.y:318
		{
			mtailVAL.n = &binaryExprNode{lhs: mtailDollar[1].n, rhs: mtailDollar[4].n, op: mtailDollar[2].op}
		}
	case 61:
		mtailDollar = mtailS[mtailpt-1 : mtailpt+1]
		//line parser.y:325
		{
			mtailVAL.op = mtailDollar[1].op
		}
	case 62:
		mtailDollar = mtailS[mtailpt-1 : mtailpt+1]
		//line parser.y:327
		{
			mtailVAL.op = mtailDollar[1].op
		}
	case 63:
		mtailDollar = mtailS[mtailpt-1 : mtailpt+1]
		//line parser.y:329
		{
			mtailVAL.op = mtailDollar[1].op
		}
	case 64:
		mtailDollar = mtailS[mtailpt-1 : mtailpt+1]
		//line parser.y:331
		{
			mtailVAL.op = mtailDollar[1].op
		}
	case 65:
		mtailDollar = mtailS[mtailpt-1 : mtailpt+1]
		//line parser.y:336
		{
			mtailVAL.n = mtailDollar[1].n
		}
	case 66:
		mtailDollar = mtailS[mtailpt-2 : mtailpt+1]
		//line parser.y:338
		{
			mtailVAL.n = &unaryExprNode{pos: tokenpos(mtaillex), expr: mtailDollar[2].n, op: mtailDollar[1].op}
		}
	case 67:
		mtailDollar = mtailS[mtailpt-1 : mtailpt+1]
		//line parser.y:344
		{
			mtailVAL.n = mtailDollar[1].n
		}
	case 68:
		mtailDollar = mtailS[mtailpt-2 : mtailpt+1]
		//line parser.y:346
		{
			mtailVAL.n = &unaryExprNode{pos: tokenpos(mtaillex), expr: mtailDollar[1].n, op: mtailDollar[2].op}
		}
	case 69:
		mtailDollar = mtailS[mtailpt-1 : mtailpt+1]
		//line parser.y:353
		{
			mtailVAL.n = mtailDollar[1].n
		}
	case 70:
		mtailDollar = mtailS[mtailpt-3 : mtailpt+1]
		//line parser.y:355
		{
			mtailVAL.n = &builtinNode{pos: tokenpos(mtaillex), name: mtailDollar[1].text, args: nil}
		}
	case 71:
		mtailDollar = mtailS[mtailpt-4 : mtailpt+1]
		//line parser.y:359
		{
			mtailVAL.n = &builtinNode{pos: tokenpos(mtaillex), name: mtailDollar[1].text, args: mtailDollar[3].n}
		}
	case 72:
		mtailDollar = mtailS[mtailpt-1 : mtailpt+1]
		//line parser.y:363
		{
			mtailVAL.n = &caprefNode{tokenpos(mtaillex), mtailDollar[1].text, false, nil}
		}
	case 73:
		mtailDollar = mtailS[mtailpt-1 : mtailpt+1]
		//line parser.y:367
		{
			mtailVAL.n = &caprefNode{tokenpos(mtaillex), mtailDollar[1].text, true, nil}
		}
	case 74:
		mtailDollar = mtailS[mtailpt-1 : mtailpt+1]
		//line parser.y:371
		{
			mtailVAL.n = &stringConstNode{tokenpos(mtaillex), mtailDollar[1].text}
		}
	case 75:
		mtailDollar = mtailS[mtailpt-3 : mtailpt+1]
		//line parser.y:375
		{
			mtailVAL.n = mtailDollar[2].n
		}
	case 76:
		mtailDollar = mtailS[mtailpt-1 : mtailpt+1]
		//line parser.y:379
		{
			mtailVAL.n = &intConstNode{tokenpos(mtaillex), mtailDollar[1].intVal}
		}
	case 77:
		mtailDollar = mtailS[mtailpt-1 : mtailpt+1]
		//line parser.y:383
		{
			mtailVAL.n = &floatConstNode{tokenpos(mtaillex), mtailDollar[1].floatVal}
		}
	case 78:
		mtailDollar = mtailS[mtailpt-1 : mtailpt+1]
		//line parser.y:390
		{
			mtailVAL.n = &indexedExprNode{lhs: mtailDollar[1].n, index: &exprlistNode{}}
		}
	case 79:
		mtailDollar = mtailS[mtailpt-4 : mtailpt+1]
		//line parser.y:394
		{
			mtailVAL.n = mtailDollar[1].n
			mtailVAL.n.(*indexedExprNode).index.(*exprlistNode).children = append(
				mtailVAL.n.(*indexedExprNode).index.(*exprlistNode).children,
				mtailDollar[3].n.(*exprlistNode).children...)
		}
	case 80:
		mtailDollar = mtailS[mtailpt-1 : mtailpt+1]
		//line parser.y:404
		{
			mtailVAL.n = &idNode{tokenpos(mtaillex), mtailDollar[1].text, nil, false}
		}
	case 81:
		mtailDollar = mtailS[mtailpt-1 : mtailpt+1]
		//line parser.y:411
		{
			mtailVAL.n = &exprlistNode{}
			mtailVAL.n.(*exprlistNode).children = append(mtailVAL.n.(*exprlistNode).children, mtailDollar[1].n)
		}
	case 82:
		mtailDollar = mtailS[mtailpt-3 : mtailpt+1]
		//line parser.y:416
		{
			mtailVAL.n = mtailDollar[1].n
			mtailVAL.n.(*exprlistNode).children = append(mtailVAL.n.(*exprlistNode).children, mtailDollar[3].n)
		}
	case 83:
		mtailDollar = mtailS[mtailpt-5 : mtailpt+1]
		//line parser.y:424
		{
			mp := markedpos(mtaillex)
			tp := tokenpos(mtaillex)
			pos := MergePosition(&mp, &tp)
			mtailVAL.n = &patternConstNode{pos: *pos, pattern: mtailDollar[4].text}
		}
	case 84:
		mtailDollar = mtailS[mtailpt-3 : mtailpt+1]
		//line parser.y:434
		{
			mtailVAL.n = mtailDollar[3].n
			d := mtailVAL.n.(*declNode)
			d.kind = mtailDollar[2].kind
			d.hidden = mtailDollar[1].flag
		}
	case 85:
		mtailDollar = mtailS[mtailpt-0 : mtailpt+1]
		//line parser.y:444
		{
			mtailVAL.flag = false
		}
	case 86:
		mtailDollar = mtailS[mtailpt-1 : mtailpt+1]
		//line parser.y:448
		{
			mtailVAL.flag = true
		}
	case 87:
		mtailDollar = mtailS[mtailpt-2 : mtailpt+1]
		//line parser.y:455
		{
			mtailVAL.n = mtailDollar[1].n
			mtailVAL.n.(*declNode).keys = mtailDollar[2].texts
		}
	case 88:
		mtailDollar = mtailS[mtailpt-2 : mtailpt+1]
		//line parser.y:460
		{
			mtailVAL.n = mtailDollar[1].n
			mtailVAL.n.(*declNode).exportedName = mtailDollar[2].text
		}
	case 89:
		mtailDollar = mtailS[mtailpt-1 : mtailpt+1]
		//line parser.y:465
		{
			mtailVAL.n = &declNode{pos: tokenpos(mtaillex), name: mtailDollar[1].text}
		}
	case 90:
		mtailDollar = mtailS[mtailpt-1 : mtailpt+1]
		//line parser.y:469
		{
			mtailVAL.n = &declNode{pos: tokenpos(mtaillex), name: mtailDollar[1].text}
		}
	case 91:
		mtailDollar = mtailS[mtailpt-1 : mtailpt+1]
		//line parser.y:476
		{
			mtailVAL.kind = metrics.Counter
		}
	case 92:
		mtailDollar = mtailS[mtailpt-1 : mtailpt+1]
		//line parser.y:480
		{
			mtailVAL.kind = metrics.Gauge
		}
	case 93:
		mtailDollar = mtailS[mtailpt-1 : mtailpt+1]
		//line parser.y:484
		{
			mtailVAL.kind = metrics.Timer
		}
	case 94:
		mtailDollar = mtailS[mtailpt-1 : mtailpt+1]
		//line parser.y:488
		{
			mtailVAL.kind = metrics.Text
		}
	case 95:
		mtailDollar = mtailS[mtailpt-2 : mtailpt+1]
		//line parser.y:495
		{
			mtailVAL.texts = mtailDollar[2].texts
		}
	case 96:
		mtailDollar = mtailS[mtailpt-1 : mtailpt+1]
		//line parser.y:502
		{
			mtailVAL.texts = make([]string, 0)
			mtailVAL.texts = append(mtailVAL.texts, mtailDollar[1].text)
		}
	case 97:
		mtailDollar = mtailS[mtailpt-1 : mtailpt+1]
		//line parser.y:507
		{
			mtailVAL.texts = make([]string, 0)
			mtailVAL.texts = append(mtailVAL.texts, mtailDollar[1].text)
		}
	case 98:
		mtailDollar = mtailS[mtailpt-3 : mtailpt+1]
		//line parser.y:512
		{
			mtailVAL.texts = mtailDollar[1].texts
			mtailVAL.texts = append(mtailVAL.texts, mtailDollar[3].text)
		}
	case 99:
		mtailDollar = mtailS[mtailpt-3 : mtailpt+1]
		//line parser.y:517
		{
			mtailVAL.texts = mtailDollar[1].texts
			mtailVAL.texts = append(mtailVAL.texts, mtailDollar[3].text)
		}
	case 100:
		mtailDollar = mtailS[mtailpt-2 : mtailpt+1]
		//line parser.y:525
		{
			mtailVAL.text = mtailDollar[2].text
		}
	case 101:
		mtailDollar = mtailS[mtailpt-4 : mtailpt+1]
		//line parser.y:532
		{
			mtailVAL.n = &decoDefNode{pos: markedpos(mtaillex), name: mtailDollar[3].text, block: mtailDollar[4].n}
		}
	case 102:
		mtailDollar = mtailS[mtailpt-3 : mtailpt+1]
		//line parser.y:539
		{
			mtailVAL.n = &decoNode{markedpos(mtaillex), mtailDollar[2].text, mtailDollar[3].n, nil, nil}
		}
	case 103:
		mtailDollar = mtailS[mtailpt-0 : mtailpt+1]
		//line parser.y:549
		{
			glog.V(2).Infof("position marked at %v", tokenpos(mtaillex))
			mtaillex.(*parser).pos = tokenpos(mtaillex)
		}
	case 104:
		mtailDollar = mtailS[mtailpt-0 : mtailpt+1]
		//line parser.y:559
		{
			mtaillex.(*parser).inRegex()
		}
	}
	goto mtailstack /* stack new state and value */
}
