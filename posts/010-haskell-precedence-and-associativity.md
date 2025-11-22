---
title: "Haskell 토막글: 연산 우선순위와 결합 방향"
published: 2024-10-12T11:48:00+09:00
---

> **참고**: 영어로 연산 우선순위는 precedence, 결합 방향은 associativity다.

Haskell에서는 **function application**과 **operation application**을 구분한다.
Function application의 예시로는 `show 123`이 있고, operation application의
예시로는 `1 + 2`, ``3 `div` 2`` 등이 있다.

결합 우선순위는 function application이 operation application보다 항상 높다.
다음은 간단한 예시다.

```haskell
show 1 + 3    -- Error; (show 1) + 3
show (1 + 3)  -- Ok
show $ 1 + 3  -- Ok, ($) is an operator; show $ (1 + 3)
```

같은 우선순위 내에서는 **결합 방향(associativity)**에 따라 괄호를 묶는다.
Haskell에서는 결합 방향을 왼쪽(left associative), 오른쪽(right associative),
정의되지 않음(non-associative)의 3가지 경우로 나눌 수 있다.

Function application은 모두 left associative다. 예를 들어, `f1 f2 f3`은
`((f1 f2) f3)`와 같이 왼쪽부터 괄호로 묶인다.

Operation application은 결합 방향을 사용자 정의할 수 있다. 연산자의 결합 방향은
연산이 두 개 이상이어서 ``a `op1` b `op2` c``{.haskell}와 같은 형태일 때 괄호를
치는 방법을 생각하면 결합 방향을 이해할 수 있다. 결합 방향은 `infixl`,
`infixr`, `infix` 세 가지로 정의할 수 있다.

`infixl`의 예시로 `+`, `-`, `/`, `*`와 같은 산술 연산자를 생각해볼 수 있다.
다음 예시를 살펴보자.

```haskell
1 + 2 - 3 - 4 == (((1 + 2) - 3) - 4)
1 * 2 * 3 * 4 == (((1 * 2) * 3) * 4)
```

모두 왼쪽부터 괄호를 치는 것을 알 수 있다. 위 예시는 결합 우선순위가 모두 같은
경우였다. 결합 우선순위가 다른 경우를 살펴보자.

```haskell
1 + 2 + 3 * 4 == ((1 + 2) + (3 * 4))
```

`+`는 `infixl 6`, `*`는 `infixl 7`이므로 `*`가 우선순위가 높다는 점을
알아두자. 이 경우는 우선순위가 높은 연산이 먼저 결합되고, 그 이후 `1`, `2`,
`(3 * 4)`가 왼쪽부터 괄호로 결합됨을 알 수 있다.

`infixr`과 `infix`의 예시도 살펴보자. `infixr`은 right associative이므로
오른쪽부터 괄호를 묶는다. 예시로 `$`, `.`, `->`가 있다.

```haskell
-- Note the precedence and associativity for the following operators:
-- infixr 0 $
-- infixr 9 .

-- TFAE:
show . sum . map read $ ["1", "2"]
show . sum . (map read) $ ["1", "2"]      -- Function application has higher precedence.
(show . sum . (map read)) $ ["1", "2"]    -- (.) has higher precedence than ($).
(show . (sum . (map read))) $ ["1", "2"]  -- (.) is right-associative.
-- Result: "3" :: String
```

`infix`의 예시로는 `==`이 있다. 결합 방향이 정의되지 않으므로 명시적으로 괄호를
묶어야 한다. 예를 들어, `a == b == c`는 오류가 발생한다. `(a == b) == c` 또는
`a == (b == c)`로 명시적으로 작성해야 한다.
