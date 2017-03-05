require_relative './pattern'

Pattern.define  :TmIf,
                :TmTrue,
                :TmFalse,
                :TmZero,
                :TmSucc,
                :TmPred,
                :TmIsZero

def isnumerical(term)
  Pattern.match(term) do |m|
    m.on TmZero,    -> { true }
    m.on TmSucc(t), -> { isnumerical t }
    m.on _,         -> { false }
  end
end

def isval(term)
  Pattern.match(term) do |m|
    m.on TmTrue,  -> { true }
    m.on TmFalse, -> { true }
    m.on t,       -> { isnumerical t }
  end
end

def eval1(term)
  Pattern.match(term) do |m|
    m.on TmIf(TmTrue, t2, t3),  -> { t2 }
    m.on TmIf(TmFalse, t2, t3), -> { t3 }
    m.on TmIf(t1, t2, t3),      -> { TmIf(eval1(t1), t2, t3) }
    m.on TmSucc(t1),            -> { TmSucc(eval1(t1)) }
    m.on TmPred(TmZero),        -> { TmZero }
    m.on TmPred(TmSucc(nv1)),   -> { isnumerical(nv1) ? nv1 : nil }
    m.on TmPred(t1),            -> { TmPred(eval1(t1)) }
    m.on TmIsZero(TmZero),      -> { TmTrue }
    m.on TmIsZero(TmSucc(nv1)), -> { isnumerical(nv1) ? TmFalse : nil }
    m.on TmIsZero(t1),          -> { TmIsZero(eval1(t1)) }
  end
end

def evaln(term)
  evaln(eval1(term))
rescue Pattern::NoRuleApplies
  term
end

term = TmIf(TmTrue, TmSucc(TmPred(TmSucc(TmZero))), TmZero)
p term
p evaln(term)
