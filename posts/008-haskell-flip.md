---
title: "Haskell 토막글: flip 함수"
published: 2024-10-12T10:30:00+09:00
updated: 2025-05-07T18:30:00+09:00
---

> **요점**: `flip` 안 쓰고 infix operation 활용하는 방법이 있는데, 그게 더 흔한
> 것 같긴 하다.

타입 시그니처를 살펴보자.

```haskell
flip :: (a -> b -> c) -> b -> a -> c
```

두 개의 인자를 받는 함수에서 arg1, arg2의 적용 순서를 arg2, arg1로 바꾼다. type
signature를 같이 살펴보면 아주 명확하다. 다음은 적용 예시다.

```haskell
fn :: T1 -> T2 -> U

val2 :: T2

fn' :: T1 -> U

-- TFAE:
fn' = flip fn val2 -- flip 사용
fn' = (`fn` val2)  -- infix로 표현 후 함수로 변형
```

**Tricky case**. 인수를 꼭 2개만 받는 함수여야만 할까? 사실, `flip`의 arg1인 `a
-> b -> c`에서 `c :: * -> *`일 수 있다! `T1 -> T2 -> T3 -> U`인 함수도 `T1 ->
T2 -> (T3 -> U)`로 볼 수 있다.

```haskell
-- T1 -> T2 -> (T3 -> U)
tn :: T1 -> T2 -> T3 -> U

val2 :: T2

-- T1 -> (T3 -> U)
tn' :: T1 -> T3 -> U
tn' = flip tn val2
```

**질문**. `flip`를 실제로 많이 쓰나? 다음 예시처럼 flip을 굳이 쓰지 않고도
infix operation을 활용할 수 있다.

```haskell
fn :: T -> String -> U

fn' :: T -> U
-- TFAE:
fn' = flip fn "foobar"  -- flip 사용
fn' = (`fn` "foobar")   -- infix로 표현 후 함수로 변형
```

infix operation 활용하는게 더 흔할까? flip도 괜찮은 선택일까? 개인적으로는
함수를 infix 이항 연산자로 바라봤을 때 부자연스럽지 않다면, flip보다 연산자로
접근하는 방법이 나은 것 같다.
