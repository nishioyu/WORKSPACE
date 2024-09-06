# Your Teransformer is secretary linear解説
## 論文の概要
## 線形性がなぜ注目されるのか？​
## Transformerとは？​
## LLMの軽量化技術​
## 論文解説　Transformerの線形性の調査​

我々は、エンベッディングの線形性を評価する際に、特に変換レイヤー間のスケール分散を考慮して、そのロバスト性からこのようなアプローチを選択した。

スケール不変性を欠くL 2ノルムとは異なり、Procrustes正規化は[0,1]の範囲で境界のあるメトリックを提供する。

驚くべきことに、テストされたすべてのトランスフォーマーデコーダーのレイヤーの線形性スコアは1に近く、埋め込み変換における高度な線形性を示すことがわかった（図1）。

この現象は、残差ストリームに対する各ブロックの寄与のノルムが著しく低いという観察から部分的に説明できる（図3）。さらに、各層の埋め込み値を前の層から引くことで、メインストリーム（残差成分を含まない埋め込み）の直線性を評価すると、直線性の程度が著しく低下していることがわかる（図1）。このことは、固有の線形性が、最初に推定されるほど単純ではないことを示唆している。さらに、個々のブロックのノルムの寄与が低いため、隣接する層からの埋め込みはコサイン類似度の点で密接に整列する結果となった。もう1つの洞察は、一見線形に見えるブロックの組み合わせが、非線形な結果をもたらす可能性があるということである。Elhageら(2022)は、複雑な特徴が神経回路網の構成要素にまたがって符号化される可能性を示唆しており、トランスフォーマーの注意ヘッドに適用できる。このことは、線形変換の累積効果によって、複雑な非線形表現の符号化が可能になる可能性を示している。図9を見ると、OPT-1.3Bのいくつかの層ではL 2エラーの分布が長く尾を引いており、これは非線形性のスパイクがまだまばらに残っていることを意味する。Borseら(2023)は、効率的な推論のために、モデルパラメータの疎なサブセットをどのように動的に活性化できるかを研究しており、線形アーキテクチャが主流の中で、特定の非線形相互作用がモデルの機能にとって重要であるという考えを支持している。

This phenomenon can be partly explained by the observation that the norm of each block’s contribution to the residual stream is remarkably low (Figure 3). Moreover, when assessing the linearity of the main stream (embeddings w/o residual component) by subtracting the embedding values of each layer from the previous layer, one can notice that the degree of linearity significantly decreases (Figure 1). 
This suggests that the inherent linearity is not as straightforward as it is initially estimated. Moreover, the low norm contribution of individual blocks resulted in embeddings from adjacent layers being closely aligned in terms of cosine similarity.

One more insight is that the combination of seemingly linear blocks can lead to non-linear outcomes. 
Elhage et al. (2022) suggests that complex features can be encoded across components of neural networks, applicable to attention heads in transformers. This indicates that the cumulative effect of linear transformations might enable the encoding of intricate non-linear representations.


Furthermore, our feature triggering regime hypothesis proposes that rare specific features on a few tokens with high non-linearity significantly influence model behavior — in the Figure 9 one can see that some layers of OPT-1.3B have the long tailed distribution of 𝐿_2 errors, which means that there are still sparse spikes of non-linearity.
>「Feature triggering」とは、特定の入力特徴がモデルの内部状態や出力に顕著な変化を引き起こす現象を指します。当然線形性の表れは入出力のデータセットに依存します。(今回の場合はデコーダのため入力のみに依存すると言えます。)
「さらに、私たちのfeature triggering regime仮説は、
非線形性が高いいくつかのトークン上の希少な特定の特徴がモデルの振る舞いに大きな影響を与えると提案しています。
図9では、OPT-1.3Bのいくつかの層が長い尾の分布を持つ𝐿_2エラーを示しており、これは非線形性のまばらなスパイクが依然として存在することを意味します。」

この部分では、特定の入力特徴（"feature triggering regime"）がモデルの出力に顕著な影響を与える可能性があること、特にその特徴がまれで非線形性が高い場合に顕著であることが述べられています。また、モデルの特定の層が、長い尾を持つエラー分布を示すことから、これらの特徴がモデル全体の振る舞いにどのように影響しているかが示されています。これは、モデルがこれらの特異な入力に対してどのように反応しているかを理解するための重要な洞察を提供します。

Borse et al. (2023) explored how a sparse subset of model parameters can be dynamically activated for efficient inference, supporting the idea that within predominantly linear architectures, certain non-linear interactions are crucial for model functionality.



この論文の該当部分では、公開されている中間チェックポイントを持つオープンソースモデルと、小規模データセットでトレーニングされたカスタムモデルの両方における線形性の動態を検討しています。

特に、モデルのトレーニングの異なる段階における主要なストリーム（残差成分を含む文脈化された埋め込み）の線形性の動態を理解することを目指しています。

事前学習中は線形性が平均的に減少する傾向があり、ファインチューニング中は逆に線形性が増加することが観察されており、
タスク特有のファインチューニングがトランスフォーマーモデルの線形特性を強化し、増幅する傾向にあることを示しています。


Super Glue/MultiRC と Super Glue/BoolQ は、自然言語処理（NLP）の能力を評価するためのベンチマークタスクです。これらはSuperGLUEベンチマークの一部であり、モデルが複雑な質問応答や文章理解タスクにどのように対応するかを測定します。

Super Glue/MultiRC（Multiple Choice Reading Comprehension）は、与えられた文書に基づいて複数の選択肢から正しい回答を選ぶタスクです。
Super Glue/BoolQ（Boolean Questions）は、文書を読んで提供されたYes/No形式の質問に答えるタスクです。
これらのベンチマークは、特にテキストの理解と情報抽出の能力を深く評価することを目的としています。




## 論文解説　線形性改善手法の導入​


## 論文解説　新しいPruning 手法の導入​
## 論文解説　まとめ
## 論文解説　本研究の限界と展望​



