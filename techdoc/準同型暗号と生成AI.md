# 準同型暗号についての概要

### 準同型暗号について

---

#### **概要**
準同型暗号（Homomorphic Encryption）は、暗号化されたデータに対して直接演算を行うことを可能にする暗号技術です。この技術により、データを復号化することなく安全に処理ができ、プライバシー保護が必要な分野（例: 医療データ、金融データ、AIのモデルトレーニングなど）で活用が期待されています。

---

#### **定義**
準同型暗号は、以下の条件を満たす暗号方式です。

1. **暗号化されたデータ上での演算**  
   暗号化されたデータに対して特定の演算（加算、乗算など）を行った結果を復号化すると、元のデータにその演算を行った結果と一致します。

2. **演算の種類**  
   - **部分準同型暗号**: 特定の演算（加算または乗算）のみ対応。
   - **完全準同型暗号（FHE）**: 加算と乗算の両方が可能。

---

#### **暗号実現方法の理論的な説明**

##### **数学的な背景**
準同型暗号は以下のような数学理論に基づいています：
- **環論と体論**: 準同型特性を持つ演算を定義。
- **格子理論**: 暗号化方式の安全性を保証。
- **RSA暗号やPaillier暗号の特性**: 一部の既存方式の特性から部分準同型が導出可能。

例:  
RSA暗号では以下が成立：
\[ E(m_1) \cdot E(m_2) = E(m_1 \cdot m_2) \]  
Paillier暗号では加算準同型性：
\[ E(m_1) \cdot E(m_2) = E(m_1 + m_2) \]

##### **なぜ暗号化できるのか**
暗号化方式は、鍵（公開鍵または秘密鍵）に基づいて暗号化を行い、暗号文空間内で計算を行っても復号後の値が元の平文演算に対応するように設計されています。数学的には、暗号化と復号の関数が以下のような性質を持ちます：

- 暗号化: \( E(x) \)
- 復号化: \( D(E(x)) = x \)
- 演算: \( E(x) \star E(y) = E(x \odot y) \)

##### **計算量的な課題**
準同型暗号の主要な課題は以下の通りです：
1. **計算コスト**: 暗号文の演算は非常に計算負荷が高く、実用的なアプリケーションでの利用が制限される。
2. **暗号文サイズ**: 加算や乗算を繰り返すと暗号文が肥大化する。
3. **遅延**: 特に完全準同型暗号（FHE）の場合、計算が数百倍～数千倍遅くなる。

---

#### **代表的なライブラリやフレームワーク**

##### 1. **Microsoft SEAL**
- **概要**: Microsoftが提供する準同型暗号ライブラリ。学術研究や商用利用を想定。
- **メイン機能**: 完全準同型暗号（FHE）をサポート。加算・乗算演算が可能。
- **提供企業**: Microsoft
- **公開日**: 2015年
- **URL**: [Microsoft SEAL GitHub](https://github.com/microsoft/SEAL)

##### 2. **PySEAL**
- **概要**: SEALのPythonバインディング。Pythonで簡単に操作可能。
- **メイン機能**: Microsoft SEALの機能をPython環境で利用可能。
- **提供企業**: OpenMinedなど。
- **URL**: [PySEAL](https://github.com/OpenMined/PySEAL)

##### 3. **HElib**
- **概要**: IBMが提供するライブラリ。効率的な準同型暗号を実現。
- **メイン機能**: ヘルパー機能やバッチ処理機能で効率化。
- **提供企業**: IBM
- **公開日**: 2013年
- **URL**: [HElib GitHub](https://github.com/homenc/HElib)

##### 4. **TenSEAL**
- **概要**: AIや機械学習向けに特化した準同型暗号ライブラリ。
- **提供企業**: OpenMined
- **URL**: [TenSEAL](https://github.com/OpenMined/TenSEAL)

---

#### **実装例**

##### **Microsoft SEALを使用した暗号化と復号化**

```python
import seal

# Contextの作成
parms = seal.EncryptionParameters(seal.SCHEME_TYPE.BFV)
parms.set_poly_modulus_degree(8192)
parms.set_coeff_modulus(seal.CoeffModulus.BFVDefault(8192))
parms.set_plain_modulus(256)

context = seal.SEALContext(parms)

# 鍵生成
keygen = seal.KeyGenerator(context)
public_key = keygen.public_key()
secret_key = keygen.secret_key()
encryptor = seal.Encryptor(context, public_key)
decryptor = seal.Decryptor(context, secret_key)

# 暗号化と復号化
plaintext = seal.Plaintext("6")
ciphertext = seal.Ciphertext()
encryptor.encrypt(plaintext, ciphertext)

# 復号化
result = seal.Plaintext()
decryptor.decrypt(ciphertext, result)

print("Decrypted Result:", result.to_string())
```

---

#### **AIやLLMへの応用**

##### **応用ユースケース**
1. **プライバシー保護されたモデルトレーニング**
   - ユーザーのデータを暗号化したままモデルトレーニングを実施。
2. **秘匿データによる推論**
   - LLMが暗号化データに基づいて推論を提供。

##### **ユースケースでできること**
- ユーザーデータを開示せずにAIモデルを学習。
- データ漏洩のリスクを軽減。

##### **簡単な実装例**
TenSEALを用いた例：
```python
import tenseal as ts

# Context作成
context = ts.context(ts.SCHEME_TYPE.CKKS, poly_modulus_degree=8192, coeff_mod_bit_sizes=[60, 40, 40, 60])
context.global_scale = 2**40

# 暗号化
data = [1.0, 2.0, 3.0]
encrypted_vector = ts.ckks_vector(context, data)

# 加算
result = encrypted_vector + [4.0, 5.0, 6.0]
decrypted_result = result.decrypt()

print("Decrypted Result:", decrypted_result)
```

##### **今後の課題**
1. **計算効率の向上**: 実運用可能な計算速度を達成する。
2. **暗号文サイズの削減**: ストレージ要件の最適化。
3. **標準化**: ライブラリ間の互換性向上。

---

準同型暗号は、セキュリティとプライバシーの両立を可能にする画期的な技術です。AIやビッグデータの分野での応用が広がる中で、計算効率や実用性を高める研究が進められています。


# 準同型暗号についての論文調査

論文「A Survey on Private Transformer Inference」では、トランスフォーマーモデルの推論におけるプライバシー保護手法を包括的に調査しています。特に、ユーザーのデータプライバシーを守りながらモデル推論を行うための技術として、準同型暗号（Homomorphic Encryption, HE）や安全なマルチパーティ計算（Secure Multi-Party Computation, MPC）などの暗号技術の適用が検討されています。

**準同型暗号（HE）のAIへの適用**

HEは、データを暗号化したまま特定の計算を直接行うことを可能にする暗号技術です。これにより、ユーザーの入力データを暗号化した状態でサーバーに送信し、サーバー側で暗号化されたデータに対してモデル推論を実行し、結果を暗号化されたままユーザーに返すことができます。ユーザーはその結果を復号することで、プライバシーを保護しつつ推論結果を得ることができます。

**計算量に関する課題**

HEを用いた推論は、暗号化と復号化のオーバーヘッド、および暗号化されたデータ上での計算の複雑さにより、計算コストが高くなる傾向があります。特に、トランスフォーマーモデルのような大規模で複雑なモデルに対しては、計算資源と時間の負担が増大します。これにより、リアルタイムでの推論や大規模データの処理が難しくなる可能性があります。

**安全性に関する考察**

HEは、データを暗号化したまま計算を行うため、データのプライバシー保護に優れています。しかし、暗号化方式の選択や鍵管理、実装の際の脆弱性など、適切なセキュリティ対策が求められます。また、HE自体の暗号強度も考慮する必要があります。

**今後の課題**

1. **計算効率の改善**: HEを用いた推論の計算コストを削減し、実用的な応答時間を実現することが求められます。

2. **モデルの最適化**: HEに適したモデルアーキテクチャの設計や、暗号化されたデータ上で効率的に動作するアルゴリズムの開発が必要です。

3. **セキュリティの強化**: HEの実装における潜在的な脆弱性を特定し、対策を講じることで、全体のセキュリティを向上させる必要があります。

4. **標準化と互換性**: 異なるHEライブラリやフレームワーク間の互換性を確保し、広範な適用を可能にする標準化が求められます。

これらの課題に対処することで、HEを活用したプライバシー保護型のトランスフォーマーモデル推論が、より実用的で広範な応用が可能になると期待されます。 