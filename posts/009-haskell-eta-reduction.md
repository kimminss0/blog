---
title: "Haskell 토막글: η-reduction과 pointfree programming"
published: 2024-10-12T11:22:00+09:00
updated: 2025-05-07T18:30:00+09:00
---

> **요점**: η-reduction을 남발하면 가독성 떨어지는데, 적절하게 쓰면 깔끔하다.

**η-reduction**은 함수의 인자를 명시하지 않고 함수를 표현하는 방법이다. 예를
들어, `f x = g x`는 간단하게 `f = g`로 표현할 수 있다. 주로 함수형
프로그래밍에서 함수의 간결한 작성과 가독성을 위해 자주 사용한다.

```haskell
-- Ex) makeGreeting "Hello" "Minseo" => "Hello Minseo"
makeGreeting :: String -> String -> String
makeGreeting salutation person =
  salutation <> " " <> person

-- Apply eta reduction once
makeGreeting1 :: String -> String -> String
makeGreeting1 salutation = ((salutation <> " ") <>)

-- Apply eta reduction twice
makeGreeting2 :: String -> String -> String
makeGreeting2 = (<>) . (<> " ")
```

인자를 명시적으로 사용해 함수를 정의하는 방식을 **pointful** 스타일이라고 하며,
인자를 생략하고 기존 함수들의 조합만으로 새로운 함수를 정의하는 방식을
**pointfree** 스타일이라고 한다.

위의 예시에서는 원본 함수가 pointful, eta-reduction 적용한 1, 2번이 pointfree
스타일이다. 그런데 pointful한 원본이 가장 읽기 쉽지 않나? 그냥 아래처럼 명확한
경우만 η-reduction 써야겠다.

```haskell
addOne :: Int -> Int
addOne x = x + 1

-- Apply eta reduction; much clearer
addOne1 :: Int -> Int
addOne1 = (+ 1)
```
