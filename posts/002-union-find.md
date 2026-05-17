---
title: "Union-find"
published: 2024-09-14T17:00:00+09:00
pinned: true
---

> PS(Problem Solving) 문제 풀이를 위해 작성한 글입니다. 이론적인 설명은
> 배제했습니다.

## 개요

Union-find는 disjoint set[^1]의 collection을 나타내는 자료구조다. 다르게
표현하면, **집합의 파티션**을 나타내는 자료구조다.

Union-find의 주요 개념은 다음과 같다.

- Union-find는 집합을 트리로 나타낸다.
- `union`은 트리를 합치는 연산이다.
- `find`는 트리의 root를 찾는 연산이다.
- union by rank, path compression으로 최적화한다.
- rank는 height와 유사하지만 다른 개념이다.
- path splitting과 path halving은 메모리 효율적인 경로 압축 알고리즘이다.

[^1]: 서로소 집합. 공통 원소가 없는 두 집합이다.

## 지원하는 연산

Union-find API는 다음 연산을 지원한다.

- `makeset(x)`: x를 유일한 원소로 두는 새로운 집합 생성. 자료구조에 새로운
  원소를 추가할 때 사용한다.
- `find(x)`: x가 속한 집합을 반환
- `union(x, y)`: x, y가 속한 두 집합을 병합

Union-find는 **집합을 트리 형태로 구현**한다. `union`은 트리를 합치는 연산이고,
`find`는 트리의 root를 찾는 연산이다. 일반적으로 다음과 같이 구현한다.

- `find(x)`는 root에 도달할 때까지 parent를 타고 올라간다.
- `union(x, y)`는 두 트리의 root를 찾아 어느 하나를 다른 하나의 자식으로
  편입한다.

## 최적화 기법

`find` 연산은 flat한 트리에서 더 빠르다. 최악의 경우 트리가 리스트와 같을 수
있으며, 이 경우 `find`는 모든 노드를 순회한다. 트리의 height를 낮게 유지하는
최적화 기법을 알아보겠다.

### Union by rank

`union` 연산에서 rank가 작은 tree의 root를 rank가 큰 tree의 root의 자식으로
편입한다. 이로써 트리의 rank를 낮게 유지한다.

**rank**는 height의 upper bound로, height와 일치하지는 않지만 효율을 위해
도입한다. 특징은 다음과 같다.

- 새로 초기화된 node의 rank는 0이다.
- root가 u, v인 두 트리를 병합할 때 다음을 따른다.
  - u, v의 rank가 다르면 작은 것을 큰 것의 자식으로 편입한다.
  - u, v의 rank가 같으면 어느 하나를 부모로 만들고 rank를 1 더한다.
- height와 달리 rank가 업데이트되지 않는 경우:
  - root가 아닌 node는 rank를 업데이트하지 않는다.
  - [Path compression](#path-compression) 과정에서 height가 변해도 rank를
    업데이트하지 않는다.

### 경로 압축(Path compression)

`find` 연산 과정에서 트리를 평탄화(flatten)하는 작업을 같이 수행한다. `find(x)`
호출 시, x와 그 조상들을 한 번에 root로 직접 연결한다. 알고리즘의 설명은
후술한다.

## 의사코드

다음은 `makeset(x)`의 의사코드다.

```
function makeset(x)
    x.parent := x
    x.rank := 0
```

다음은 union by rank로 구현한 `union(x, y)`의 의사코드다.

```
function union(x, y)
    root_x := find(x)
    root_y := find(y)
    if rank(root_x) > rank(root_y)
        root_y.parent := root_x
    else
        root_x.parent := root_y
        if root_x.rank == root_y.rank
            root_y.rank := root_y.rank + 1
```

다음은 `find(x)`의 의사코드다. Path compression을 적용하지 않았다.

```
function find(x)
    while x ≠ x.parent
        x := x.parent
    return x
```

Path compression을 구현한 `find`의 의사코드는 다음과 같다.

```
function find(x)
    if x.parent ≠ x
        x.parent := find(x.parent)
        return x.parent
    else
        return x
```

재귀 호출로 구현하므로 call stack이 쌓이며 메모리 사용량이 늘어날 수 있다. 재귀
없이 구현하는 방법은 다음과 같다.

```
function find(x)
    root := x
    while root.parent ≠ root
        root := root.parent

    while x.parent ≠ root
        parent := x.parent
        x.parent := root
        x := parent

    return root
```

메모리 사용량이 상수값으로 줄어들었다. 다만, root를 찾기 위해 첫 번째 경로
탐색이, path compression을 위한 두 번째 경로 탐색이 발생한다.

한 번만 탐색하는 알고리즘 또한 있다. 단 완전한 경로 압축은 아니고, 메모리 사용
측면에서의 절충안이다. path splitting과 path halving이 있다.

아래는 path splitting 알고리즘이다.

```
function find(x)
    while x.parent ≠ x
        (x, x.parent) := (x.parent, x.parent.parent)
    return x
```

아래는 path halving 알고리즘이다.

```
function find(x)
    while x.parent ≠ x
        x.parent := x.parent.parent
        x = x.parent
    return x
```

Path splitting은 경로상 모든 부모 노드를 조부모로 연결한다. Path halving은 모든
노드가 아니라 두 번째마다 부모를 조부모로 연결한다. path splitting이 더
공격적으로 경로를 압축하나, 구현이 아주 조금 더 복잡하고 성능 차이는 거의 없다.
따라서 path halving 또한 선호된다.

## 참조

- [Wikipedia - Disjoint-set data structure](https://en.wikipedia.org/wiki/Disjoint-set_data_structure)
- [블로그 1](https://rntlqvnf.github.io/lecture%20notes/algorithm-5th-week-1/)
