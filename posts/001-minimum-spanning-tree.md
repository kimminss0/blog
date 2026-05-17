---
title: 최소신장트리(Minimum spanning tree, MST)
published: 2024-09-13T17:00:00+09:00
katex: true
header-includes: |
  \newcommand{\abs}[1]{\left| #1 \right|}
---

> PS(Problem Solving) 문제 풀이를 위해 작성한 글입니다. 이론적인 설명은
> 배제했습니다.

## 개요

최소신장트리(MST, Minimum spanning tree)는 그래프의 **모든 정점을 포함하는
subgraph 중 weight의 합이 최소인 트리**이다. 즉, MST는 그래프의 모든 정점을
최소비용으로 연결하는 트리다.

## 전제

다음과 같은 가정을 전제로 한다.

- Connected graph에서 논한다.
- 간선 가중치가 **음수, 0**일 수 있다.

추가로, MST의 유일성을 보장하기 위해 간선 가중치가 모두 달라야 한다는 조건을
추가할 수 있다. 간선 가중치가 모두 다른 값을 가지면 MST가 유일하고, 그 역도
성립한다. 즉, 가중치가 같은 간선들이 있으면 MST를 여러 개 가질 수 있다.

## 알고리즘

다음은 MST를 구하는 알고리즘 목록이다.

- [Kruskal's algorithm](/posts/3/kruskal-algorithm)
- [Prim's algorithm](/posts/4/prim-algorithm)

각 알고리즘을 비교하면 다음과 같다.

| 알고리즘 |  사용 환경  |        시간 복잡도         |
|----------|-------------|----------------------------|
| Kruskal  | 희소 그래프 |$\text{O}(\abs{E}\log{\abs{E}})$    |
| Prim     | 밀집 그래프 |$\text{O}(\abs{E}\log{\abs{V}})$[^1]|

[^1]: binary heap으로 구현 시

## 참조

- [프린스턴 대학 알고리즘 강의 - Minimum Spanning Trees](https://algs4.cs.princeton.edu/43mst/)
