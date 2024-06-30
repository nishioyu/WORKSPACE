# 拡散モデルの学習

## 拡散モデルの概要
拡散モデルの代表例は2022:DALL-E2, Midjourney, Stable Diffusionである。
拡散モデルは従来の生成モデルに比べて優れている点がある。
1. 学習が安定している
2. 難しい生成問題を簡単な部分生成問題に自動分解し、難しいデータ対象も生成可能
3. さまざまな条件付き生成を後付けのプラグインによって実現でき、条件付きの強さを自由に設定できる
4. 生成における対称性、不変性を組み込むことが可能

拡散モデルはデータに確率的なノイズをくわえていき、それを逆向きにた辿ることで、ある確率的なノイズからデータの生成を得るというアイディアに基づいている。

また拡散モデルは潜在変数モデルに基づく生成モデルとみなすことが可能。
つまり、最初のノイズや途中のノイズを加えたデータを潜在変数とみなすことができる。