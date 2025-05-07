---
title: Kruskal 알고리즘
published: 2024-09-18T17:00:00+09:00
updated: 2025-05-07T18:00:00+09:00
---

> PS(Problem Solving) 문제 풀이를 위해 작성한 글입니다. 이론적인 설명은
> 배제했습니다.

## 개요

Kruskal 알고리즘은 [minimum spanning tree](/posts/1/minimum-spanning-tree)를
찾는 그리디 알고리즘이다. 간선을 정렬하여 최소 비용 간선부터 선택하며, 이
과정에서 사이클이 형성되지 않도록 한다. 사이클의 형성 여부를 판별하기 위해
Union-find 자료구조를 사용한다.

## 알고리즘

알고리즘의 핵심은 매 순간 **최소 비용 간선을 선택**하면서, **사이클이 형성되지
않도록** 주의하는 것이다.

매번 최소 비용 간선을 고른다는 점에서 알고리즘이 탐욕적(greedy)임을 알 수 있다.
이런 접근이 가능한 이유는 그래프의 cut property에서 기인한다. 증명 과정은
생략하겠다.

사이클을 피하려면 같은 컴포넌트에 속하는 정점 사이에는 간선을 추가하면 안 된다.
다른 표현으로는, 두 정점 간의 연결성(connectivity)[^1]를 검사하여
disconnected인 경우에만 간선을 추가할 수 있다. 이와 같은 연결성 문제를
효율적으로 다루기 위해 **union-find** 자료구조를 사용한다.

위 내용을 정리하면 다음과 같다.

- 간선을 정렬하여 가중치가 작은 간선부터 선택한다.
- 해당 간선을 추가할 때 사이클이 형성되는지 확인한다. 사이클 형성 여부를
  확인하기 위해 union-find 자료구조를 사용한다.

[^1]:
    무방향 그래프에서 두 정점 v와 u 사이에 경로가 존재하면, 이들을
    **연결되었다(connected)**고 정의한다. 반대로, 경로가 존재하지 않으면
    **연결되지 않았다(disconnected)**고 정의한다.

## 자료구조

### Union-find

[Union-find](/posts/2/union-find)는 disjoint set[^2]의 collection을 나타내는
자료구조다. 다음 3가지 연산을 지원한다.

- `makeset(x)`: x를 유일한 원소로 두는 새로운 집합 생성. 초기화를 위해 사용.
- `find(x)`: x가 속한 집합을 반환
- `union(x, y)`: x, y가 속한 두 집합을 병합

그래프의 각 컴포넌트는 하나의 disjoint set으로 볼 수 있다. 두 정점이 서로 다른
컴포넌트에 있는지 확인할 때는 `find` 연산을 사용하고, 간선 {u, v}[^3]를 선택한
이후 두 컴포넌트를 병합하기 위해 `union(u, v)` 연산을 사용할 수 있다.

> **Note.** Union-find 자료구조는 그래프를 완전히 표현하지 않는다. 정점과
> 컴포넌트의 소속 관계만 다룰 뿐, 간선 정보는 직접적으로 관리하지 않는다.

[^2]: 서로소 집합. 공통 원소가 없는 두 집합을 말한다.

[^3]: u, v를 양 끝 정점으로 하는 간선

## 의사코드

위에서 논의한 내용을 종합하여 Kruskal 알고리즘을 단계별로 나누면 다음과 같다.

- 그래프의 모든 컴포넌트가 하나의 정점만을 가지도록 초기화한다.
- 가중치가 작은 간선부터 순회한다.
  - 양 끝 정점이 서로 연결되어있는지 확인한다.
  - 연결되지 않았으면, 두 정점이 속하는 컴포넌트를 병합하고 간선은
    저장해둔다.
- 간선을 모두 방문한 이후, 저장해둔 간선들은 MST를 이룬다.

다음은 알고리즘의 의사코드다.

```
function Kruskal(G = (V, E))
    X := {}
    for each vertex u ∈ V
        MAKESET(u)
    for each {u, v} ∈ E ordered by weight increasing
        if FIND(u) ≠ FIND(v)
            add {u, v} to X
            UNION(u, v)
    T := (V, X)
    return T
```

> Union-find의 의사코드는 [해당 문서][union-find-code] 를 참조한다.

[union-find-code]: /posts/2/union-find#pseudocode

## 구현

> 구현은 BOJ 문제 답안을 참고한다.

- [\[BOJ\] #1197 최소 스패닝 트리](/posts/5/boj-1197#kruskal)

## 기타

### Kruskal vs Prim

- Prim 알고리즘 문서 참조 [링크](/posts/4/prim-algorithm#kruskal-vs-prim)

## 참조

- [프린스턴 대학 알고리즘 강의 - Minimum Spanning Trees](https://algs4.cs.princeton.edu/43mst/)
- [블로그 1](https://rntlqvnf.github.io/lecture%20notes/algorithm-5th-week-1/)
