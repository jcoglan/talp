#### 2.1.1: notation

A set is written:

-   ∅ for the empty set
-   {𝑥, 𝑦, …} to enumerate elements
-   {𝑥 ∈ 𝑆 | …} is a *comprehension*, meaning all the members of 𝑆 that
    satisfy some condition given after the |

𝑆 ∖ 𝑇 is the difference between two sets i.e. {𝑠 ∈ 𝑆 | 𝑠 ∉ 𝑇}.

|𝑆| is the size of 𝑆, the number of elements it contains.

𝒫(𝑆) is the powerset of 𝑆; the set of all subsets of 𝑆.


#### 2.1.2: countability

The natural numbers ℕ = {0, 1, 2, 3, …}. A set is *countable* if its members
can pair up one-to-one with members of ℕ.

Counterexample: the real numbers ℝ are not countable; there is no "𝑛th real
number".


#### 2.1.3: relations

An *𝑛-place relation* is a set of n-tuples whose elements come from one of n
input sets. 𝑅 = 𝑆₁ × 𝑆₂ × … × 𝑆𝑛. Elements 𝑠₁, 𝑠₂… are *related* if (𝑠₁, 𝑠₂, …)
∈ 𝑅.

A relation is like a database table where each of its component sets is all the
unique values from one column. For example:

    +--------+-----------------+
    | Alice  | Developer       |
    | Bob    | Product Manager |
    | Claire | Designer        |
    | Daniel | Developer       |
    +--------+-----------------+

This is a 2-place relation that relates the sets 𝑆₁ = {`Alice`, `Bob`, `Claire`,
`Daniel`} and 𝑆₂ = {`Developer`, `Product Manager`, `Designer`}. `Claire` and
`Designer` are related. `Daniel` and `Product Manager` are not related.


#### 2.1.4: predicates

A *predicate* 𝑃 is a 1-place relation on 𝑆, that is 𝑃 is a subset of 𝑆. We say
𝑃(𝑠) is true if 𝑠 ∈ 𝑃.


#### 2.1.5: binary relations

A *binary relation* 𝑅 is a 2-place relation on 𝑆 and 𝑇. The notation 𝑠 𝑅 𝑡 is
equivalent to (𝑠, 𝑡) ∈ 𝑅 where 𝑠 ∈ 𝑆, 𝑡 ∈ 𝑇. If 𝑆 and 𝑇 are the same set, then 𝑅
is just a "binary relation on 𝑆".


#### 2.1.6 mixfix notaion

Some relations use mixfix notation e.g. 𝛤 ⊢ 𝑠 : 𝑇 means the same thing as (𝛤, 𝑠,
𝑇) ∈ 𝑅.


#### 2.1.7 domain and range

dom(𝑅) = {𝑠 ∈ 𝑆 | (𝑠, 𝑡) ∈ 𝑅}, that is, the domain is all the members of 𝑆 that
appear as part of a tuple in 𝑅. If you think of 𝑅 as a two-column table, the
domain is all the unique values in the first column.

range(𝑅) = {𝑡 ∈ 𝑇 | (𝑠, 𝑡) ∈ 𝑅}, that is, the range is all the members of 𝑇 that
appear as part of a tuple in 𝑅. This would be all the unique values in the
second column.

If you think of 𝑅 as a *function* from 𝑆 to 𝑇, the domain is all the possible
inputs and the range is all the possible outputs.



#### 2.1.8: functions

A relation 𝑅 on 𝑆 and 𝑇 is a *partial function* if (𝑠, 𝑡₁) ∈ 𝑅 and (𝑠, 𝑡₂) ∈ 𝑅
together imply that 𝑡₁ = 𝑡₂. That is, the relation relates each member of 𝑆 to
at most one member of 𝑇. (Multiple members of 𝑆 may be related to the same
member of 𝑇 though.)

Using the database analogy, think of this as putting a uniqueness constraint on
the first column. Or, think of the relation as a hash table where the keys are
in 𝑆 and the values in 𝑇.

𝑅 is a *total function* if it relates *every* member of 𝑆 to something, that is
dom(𝑅) = 𝑆 rather than dom(𝑅) ⊂ 𝑆.


#### 2.1.9: definedness

𝑅 is *defined* on 𝑠 if 𝑠 ∈ dom(𝑅), that is, relation maps 𝑠 to something.

𝑓(𝑥) ↑ means the function is undefined for that input.

𝑓(𝑥) ↓ means the function is defined for that input.

𝑅 may also *fail*, i.e. 𝑓(𝑥) = *fail*. *fail* is a distinct value not part of
any set; if 𝑅 can fail then its range is 𝑇 ∪ {*fail*}.


#### 2.1.10: preservation

𝑅 *preserves* 𝑃 if (𝑠, 𝑠′) ∈ 𝑅 and 𝑃(𝑠) together imply 𝑃(𝑠′). That is, when the
relation maps one value to another, a predicate that's true of the input will
also be true of the output.

For example, the predicate *even?* on the natural numbers ℕ is preseved by the
relation "add 2". Adding 2 to any even natural number gives you another even
natural number.


#### 2.2.1 reflexivity, symmetry and transitivity

𝑅 is *reflexive* if (𝑠, 𝑠) ∈ 𝑅 ∀ 𝑠 ∈ 𝑆, that is 𝑅 relates every member of 𝑆 to
itself. It may also relate members of 𝑆 to other members. For example the
*equals* function is reflexive, it only relates values to themselves (4 = 4, 4 ≠
5), but also the *less-than-or-equal* function is reflexive, it relates members
to themselves and to other values (4 ⩽ 4, 4 ⩽ 5).

𝑅 is *symmetric* if (𝑠, 𝑡) ∈ 𝑅 implies (𝑡, 𝑠) ∈ 𝑅 for all 𝑠, 𝑡 ∈ 𝑆. Equality is
symmetric; 𝑥 = 𝑦 implies 𝑦 = 𝑥.

𝑅 is *transitive* if (𝑠, 𝑡) ∈ 𝑅 and (𝑡, 𝑢) ∈ 𝑅 together imply (𝑠, 𝑢) ∈ 𝑅. For
example, the *less-than* relation is transitive; if 𝑥 < 𝑦 and 𝑦 < 𝑧 then 𝑥 < 𝑧.

𝑅 is *antisymmetric* if 𝑠 𝑅 𝑡 and 𝑡 𝑅 𝑠 together imply 𝑠 = 𝑡.



#### 2.2.2: orders

A *pre-order* is a reflexive transitive relation on 𝑆, written ⩽.

𝑠 < 𝑡 means 𝑠 ⩽ 𝑡 ∧ 𝑠 ≠ 𝑡 i.e. 𝑠 is strictly less than 𝑡.

A *partial order* is a pre-order that is also antisymmetric, for example the
familiar *less-than-or-equal* function.

A *total order* is like a partial order with the extra condition that ∀ 𝑠, 𝑡 ∈
𝑆, either 𝑠 ⩽ 𝑡 or 𝑡 ⩽ 𝑠. That means every pair of elements in the set can be
ordered relative to each other.

For example, the natural numbers have a total order, because if you pick any
pair of numbers you can say which one is greater. A graph of git commits has
total order if you try to order them by date, but only partial order if you want
to order them by ancestry.

                        B
                o-------o
      A        /
      o-------o
               \            C
                ----o-------o

Commit A is stricly *less than* B and C since it's an ancestor of both of them,
i.e. it comes before them in the change history.  B and C cannot be ordered
relative to each other; one cannot be said to have causally happened *before*
the other. (Fun fact: merge conflicts are caused by being unable to infer a
total order for lines of code when two branches modified the same file.)


#### 2.2.3: joins and meets

A *join* (or least upper bound) 𝑗 of two elements 𝑠, 𝑡 ∈ 𝑆 is an element meeting
these conditions:

1. 𝑠 ⩽ 𝑗 and 𝑡 ⩽ 𝑗
2. for any element 𝑘 satisfying condition 1, 𝑗 ⩽ 𝑘

e.g. in the natural numbers, 𝑗 is the smallest number that's greater than or
equal to 𝑠 and 𝑡.

A *meet* (or greatest lower bound) 𝑚 of two elements 𝑠, 𝑡 ∈ 𝑆 is an element
meeting these conditions:

1. 𝑚 ⩽ 𝑠 and 𝑚 ⩽ 𝑡
2. for any element 𝑘 satisfying condition 1, 𝑘 ⩽ 𝑚

e.g. in the natural numbers, 𝑚 is the largest number that's less than or
equal to 𝑠 and 𝑡.


#### 2.2.4: equivalence

An *equivalence* is a reflexive, symmetric, transitive relation.


#### 2.2.5: closure

If 𝑅 is a binary relation on 𝑆:

-   The *reflexive closure* 𝑅′ is the smallest reflexive relation that contains
    𝑅. Constuct this by adding all the pairs (𝑠, 𝑠) to 𝑅 where 𝑠 is in the
    domain of 𝑅.
-   The *transitive closure* 𝑅⁺ is the smallest transitive relation that
    contains 𝑅.


#### 2.2.9: decreasing chains

If we have a pre-order ⩽ on 𝑆, then a *decreasing chain* is a sequence 𝑠₁, 𝑠₂,
𝑠₃, … where each item is strictly less than the one that follows it.


#### 2.2.10: well-foundedness

A pre-order is *well-founded* if it contains no infinite decreasing chains, i.e.
there's some element that's smaller than all the others, e.g. zero in ℕ.
Whereas, a pre-order on the integers is not well-founded since you can keep
counting down towards minus infinity.

We also say 𝑆 is a *well-founded* set, meaning it has some element that behaves
like zero in ℕ with respect to ordering. In a type system it might mean a class
with no superclass, for example.
