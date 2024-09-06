
## 工数削減・生産性向上による人月ビジネスへの影響
非常に重要なポイントです。人月ビジネスモデルに依存している場合、工数削減によって売上が減少するリスクがあるのは確かです。
しかし、生成AIを活用した生産性向上には以下のようなメリットも考えられます。

### 1. 高付加価値サービスへのシフト
工数を削減して浮いた時間を使って、より高付加価値なサービスやコンサルティングにシフトすることが可能です。これにより、単純な開発作業以上の収益を見込むことができます。

### 2. クライアント満足度の向上
AIを活用することで、クライアントに対してより迅速かつ質の高いサービスを提供できるようになります。これにより、クライアントの満足度が向上し、長期的な関係構築や追加案件の獲得につながる可能性があります。

### 3. 市場競争力の強化
競合他社も同様の技術を導入する可能性があるため、NTTデータが先行して技術導入を進めることで、競争力を維持・強化できます。これにより、市場シェアを拡大することができるかもしれません。

### 4. 新たなビジネスモデルの構築
生成AIを活用することで、新たなビジネスモデルやサービスを構築する機会が生まれます。例えば、AIを使った自動化ソリューションの提供や、AIコンサルティングサービスなど、新しい収益源を開拓することが考えられます。

### 5. スケーラビリティの向上
生成AIによって作業効率が上がれば、同じリソースでより多くのプロジェクトをこなすことができるようになります。これにより、売上減少を補填するだけでなく、むしろ増収を見込むことも可能です。

### まとめ
生成AIを活用することによる工数削減と人月ビジネスモデルの矛盾は確かに存在しますが、それを上回るメリットを享受することで、長期的なビジネス成長を実現することが可能です。貴社の戦略的なビジョンや市場動向を踏まえ、適切な方向性を模索することが重要です。



# ソフトウェア開発における生成AI適用　ソースコードからの仕様復元

[GPT4-o]
以下の<prompt>に基づいて、<source>の内容のCOBOLソースコードか設計要素を抽出して日本語でまとめてください


<prompt>
You are provided with a COBOL source code. Analyze the code and provide detailed information on the following aspects:

1. **Overall Processing Summary**: Provide a high-level summary of what the entire code does. Describe the main functionality and purpose of the code.

2. **Variables**: List all the variables used in the code, including their data types and initial values (if any). Explain the role of each variable.

3. **Sub-processes and Numbering**: Identify and list all the sub-processes (sections, paragraphs, and routines) within the code. Number each sub-process and provide:
   - Name of the sub-process
   - Brief description of its purpose
   - Any inputs and outputs

4. **Sub-process Details**: For each identified sub-process, detail the steps and logic it performs. Include any key calculations, data manipulations, and control flow elements.

5. **Interface Information**: Describe the interface of each sub-process within the COBOL program. Include details about:
   - Input parameters and their expected formats
   - Output parameters and their formats

6. **External Program Calls**: List any other COBOL or external programs that are called within each sub-process. Provide details on how they are called and their purpose.

7. **Processing Flow Diagram**: Provide a flowchart or detailed description of the processing flow within the code, focusing on the sub-processes and their interactions. Highlight the sequence of execution from start to end, including any conditional branches and loops.

Here is the COBOL source code for analysis:

<source>
IDENTIFICATION DIVISION.
PROGRAM-ID. SampleCOBOLProgram.

ENVIRONMENT DIVISION.
CONFIGURATION SECTION.
SOURCE-COMPUTER. IBM-370.
OBJECT-COMPUTER. IBM-370.

INPUT-OUTPUT SECTION.
FILE-CONTROL.
    SELECT EmployeeFile ASSIGN TO 'EMPLOYEE.DAT'
    ORGANIZATION IS LINE SEQUENTIAL.

DATA DIVISION.
FILE SECTION.
FD EmployeeFile.
01 EmployeeRecord.
   05 EMP-ID              PIC X(10).
   05 EMP-NAME            PIC X(30).
   05 HOURS-WORKED        PIC 9(3)V99.
   05 HOURLY-RATE         PIC 9(2)V99.

WORKING-STORAGE SECTION.
01 WS-EMP-ID              PIC X(10).
01 WS-EMP-NAME            PIC X(30).
01 WS-HOURS-WORKED        PIC 9(3)V99.
01 WS-HOURLY-RATE         PIC 9(2)V99.
01 WS-GROSS-PAY           PIC 9(5)V99.
01 WS-TAX                 PIC 9(4)V99.
01 WS-NET-PAY             PIC 9(5)V99.
01 WS-STRING-EDIT         PIC X(50).
01 WS-DB-STATUS           PIC 9(2).

01 SWITCHES.
   05 EOF-SWITCH          PIC X VALUE 'N'.

EXEC SQL INCLUDE SQLCA END-EXEC.

PROCEDURE DIVISION.
    PERFORM INITIALIZE
    PERFORM PROCESS-FILE
    PERFORM CLOSE-FILES
    STOP RUN.

INITIALIZE.
    OPEN INPUT EmployeeFile
    MOVE ' ' TO EMP-ID, EMP-NAME, HOURS-WORKED, HOURLY-RATE.

PROCESS-FILE.
    PERFORM UNTIL EOF-SWITCH = 'Y'
        READ EmployeeFile INTO EmployeeRecord
        AT END MOVE 'Y' TO EOF-SWITCH
        NOT AT END
            MOVE EMP-ID TO WS-EMP-ID
            MOVE EMP-NAME TO WS-EMP-NAME
            MOVE HOURS-WORKED TO WS-HOURS-WORKED
            MOVE HOURLY-RATE TO WS-HOURLY-RATE
            PERFORM CALCULATE-PAY
            PERFORM EDIT-STRING
            PERFORM CHECK-INPUT
            PERFORM DB-ACCESS
            PERFORM DB-UPDATE
            PERFORM WRITE-REPORT
        END-READ
    END-PERFORM.

CALCULATE-PAY.
    COMPUTE WS-GROSS-PAY = WS-HOURS-WORKED * WS-HOURLY-RATE
    COMPUTE WS-TAX = WS-GROSS-PAY * 0.20
    COMPUTE WS-NET-PAY = WS-GROSS-PAY - WS-TAX.

EDIT-STRING.
    STRING WS-EMP-NAME DELIMITED BY SPACE
           ' worked for ' DELIMITED BY SIZE
           WS-HOURS-WORKED DELIMITED BY SPACE
           ' hours at a rate of ' DELIMITED BY SIZE
           WS-HOURLY-RATE DELIMITED BY SPACE
           ' per hour.' DELIMITED BY SIZE
           INTO WS-STRING-EDIT.

CHECK-INPUT.
    IF WS-HOURS-WORKED < 0 OR WS-HOURLY-RATE < 0
        DISPLAY 'Invalid Input Data'
        MOVE 'Y' TO EOF-SWITCH
    END-IF.

DB-ACCESS.
    EXEC SQL
        SELECT EMP_ID
        INTO :WS-EMP-ID
        FROM EMPLOYEE
        WHERE EMP_ID = :WS-EMP-ID
    END-EXEC
    IF SQLCODE = 100
        MOVE 0 TO WS-DB-STATUS
    ELSE
        MOVE 1 TO WS-DB-STATUS
    END-IF.

DB-UPDATE.
    IF WS-DB-STATUS = 1
        EXEC SQL
            INSERT INTO EMPLOYEE (EMP_ID, EMP_NAME, GROSS_PAY, TAX, NET_PAY)
            VALUES (:WS-EMP-ID, :WS-EMP-NAME, :WS-GROSS-PAY, :WS-TAX, :WS-NET-PAY)
        END-EXEC
    ELSE
        EXEC SQL
            UPDATE EMPLOYEE
            SET GROSS_PAY = :WS-GROSS-PAY,
                TAX = :WS-TAX,
                NET_PAY = :WS-NET-PAY
            WHERE EMP_ID = :WS-EMP-ID
        END-EXEC.

WRITE-REPORT.
    DISPLAY 'Employee Report: ' WS-STRING-EDIT.

CLOSE-FILES.
    CLOSE EmployeeFile.


# 出力した設計書の評価方法
評価する観点は以下のものがある。

TERASOLUNAに基づく設計書の品質
    基本的に外部設計書の品質は結合テストによって判断される。
    今回対象となるのはジョブ設計書のため、xxxテストによって品質が評価される。
    xxxテストで測定するのはxxxが正しくできているかを評価している。
    そのためxxxを入力に、xxxがxxxできているか(正しくできている割合)で評価する。
    (つまり実際に)


設計書の人間生成物との比較評価
    有識者リバース外部設計書との項目比較
        サブ処理への分け方(網羅性)
        サブ処理のラベルの正確性
        変数の抽出
        外部インタフェースの正確性

設計書のchatGPT-4の生成物の比較評価
    chatGPT-4の生成物との単純評価
    chatGPT-4によるCOBOLソースへの再変換とCOBOLソースコードとの比較評価

# COBOLの生成AI復元の外部設計書の「ただしさ」評価について
IBMのCOBOLの、バックエンド処理のソースコードを入力として考える。
生成AIによるCOBOLのジョブ設計書(外部設計書)の「ただしさ」を評価する方法を考える。
まず"COBOL"に対するジョブ設計書(外部設計書)はTERASOLUNA開発手順によると以下の内容に基づいている。
[処理関連情報]
- 処理フロー
- サブルーチンへの分割
- サブルーチンの分類
- サブルーチンの処理内容の記載

またジョブその他西尾が重要と思われる項目としては下記のものがある。
ようは外部処理、インタフェースとその中の項目に関連する情報である。
これらはTERASOLUNA開発手順のアプリケーション概要設計の処理設計以外のタスクもかんがえた際に、
重要となる項目かつCOBOLの外部設計書を構成する処理設計要素に関したものを抽出した。
[インタフェース関連情報]
- 入出力がある外部インタフェース(ファイル、DB、帳票)(外部インタフェース設計書)
- 外部インタフェースのアクセス項目(外部インタフェース設計書)
- 呼び出しCOBOLファイル(共通処理設計書)
- 呼び出しCOBOLに渡すパラメータ(共通処理設計書)

これらの情報が「ただしく」生成AIで生成できているかを考える。
今回は復元をかんがえるため、「たさしさ」の源泉はCOBOLソースコードであると考える。

したがって以下のような「正しさ」の測定方法が考えられる。
- 外部設計書を元に生成したCOBOLソースコードの「ただしさ」をはかる。
    - これは基本的にはUTの工程になる。


AsIs外部設計書であるという観点を考えると設計書の目的から考えるとそれが
「よい」ものか「ただしい」ものかどうか判断できると思われる。

設計書の目的はあくまでAsIsの外部背系書のインプットとなるドキュメントとなることである。
そのため外部設計レベルで記載がソースコードに準拠して正確に記載されている必要がある。

「ただしさ」の測定方法案

- 設計書を元にCOBOLソースを GPT-4で生成し、UTを実施する。
- 設計書の記載項目が有識者の記載項目との一致度合いを確認する
  - 検証にやや時間がかかるデメリットがあるが、もっとも設計書としての正確性を判断可能。
  - 定量項目をどうするかは後ほど検討
- 設計書の記載項目をGPT-4の記載項目との一致度合いを確認する

これらの定量スコアとして、適宜必要に応じて訂正スコアも加味して変換するのはどうか。
COBOL設計書として抽出すべき記載項目をすべて網羅し、繰り返し処理なども網羅したCOBOLソースコードを作成する。
このソースを設計書に起こして記載項目が正確かどうかを確認する。



# COBOL設計書の有識者RVによるただしさ確認の方法

まず"COBOL"に対するジョブ設計書(外部設計書)はTERASOLUNA開発手順によると以下の内容に基づいている。
[処理関連情報]
- 処理フロー
- サブルーチンへの分割
- サブルーチンの分類
- サブルーチンの処理内容の記載

またジョブその他西尾が重要と思われる項目としては下記のものがある。
ようは外部処理、インタフェースとその中の項目に関連する情報である。
これらはTERASOLUNA開発手順のアプリケーション概要設計の処理設計以外のタスクもかんがえた際に、
重要となる項目かつCOBOLの外部設計書を構成する処理設計要素に関したものを抽出した。
[インタフェース関連情報]
- 入出力がある外部インタフェース(ファイル、DB、帳票)(外部インタフェース設計書)
- 外部インタフェースのアクセス項目(外部インタフェース設計書)
- 呼び出しCOBOLファイル(共通処理設計書)
- 呼び出しCOBOLに渡すパラメータ(共通処理設計書)

したがって以下の項目がただしさ項目として確認できうる内容だと考える。



# COBOL設計書復元

## 前提
- COBOLのTERASOLUNA REENG等で復元される内部設計書相当の情報はあるものとする。(今回はソースコードから読み取る必要があるが、既に設計書として記述されているものがあると仮定)
- COBOLのTERASOLUNA手順の外部設計書のうち、共通処理の設計書は復元済みと仮定する。


## 手順
COBOLのメインプログラムが与えられたときにそれを以下の設計要素に直していく手順を作りたいと思います。

1. ソースコードのエリア(特にサブルーチンへの)への分割
2. エリアごとに処理ロジックの種類を分類
3. 処理ロジックを記述
4. 全体の処理フローを記述


1. ソースコードのエリアへの分割
    COBOLプログラムの構造は、非常に階層的で論理的な形式になっています。以下に、COBOLプログラムの基本的な構造を示します。

    ### COBOlのソースコードの基本構造

    1. **DIVISION**
    2. **SECTION**
    3. **PARAGRAPH**



    ### 1. DIVISION（区分）

    COBOLプログラムは、4つの主要なDIVISIONに分けられます。それぞれのDIVISIONは、プログラムの特定の部分や機能を定義するために使用されます。

    - **Identification Division**: プログラムの識別情報を記述します。
    - **Environment Division**: プログラムの実行環境に関する情報を記述します。
    - **Data Division**: プログラムで使用するデータを定義します。
    - **Procedure Division**: プログラムの実際の処理手順を記述します。

    ### 2. SECTION（セクション）

    SECTIONは、DIVISION内の部分を区切るために使用されます。特に、Data DivisionとProcedure DivisionでSECTIONが使用されます。

    - **Data Division内のSECTION**:
    - **File Section**: ファイルのレコードレイアウトを定義します。
    - **Working-Storage Section**: プログラムで使用する一時データを定義します。
    - **Linkage Section**: プログラム間でデータを共有するための領域を定義します。

    - **Procedure Division内のSECTION**: 複数のPARAGRAPHをまとめて一つのセクションにすることができますが、使用しない場合もあります。

    ### 3. PARAGRAPH（パラグラフ）

    PARAGRAPHは、具体的な処理手順を記述するための基本単位です。PARAGRAPHはProcedure Division内で使用され、プログラムの論理的な処理のまとまりを表します。

    **例**:
    ```cobol
    PROCEDURE DIVISION.
    initialize.
        PERFORM open-files.
        PERFORM read-initial-data.
        
    process-data.
        PERFORM read-file
        PERFORM process-records
        PERFORM write-output
    ```


    主に処理の記述に必要になるのはCOBOLのPRECEDURE DIVISIONになります。
    したがってこの内部をSECTIONとPARAGRAPHに分割して考えます。
    以下この1COBOLファイルに対してサブルーチンとはこのPROCEDURE DIVISIONの内部のPARAGRAPHのことを指します。

    まずはソースコードをこのサブルーチンの単位に分割して項番を振ります。


2. エリアごとに各処理ロジックの種類を分類
    以下は1. で分割したサブルーチンごとに中身を見ていきます。
    2-1. 分割したサブルーチンの処理が、以下のTERASOLUNA設計書記載の処理パターンのどれに該当するか確認します。⭐️
    2-2. 該当する処理かどうかを確認するためには以下のキーワードに注目してください⭐️
    2-3. もしサブルーチンが該当するパターン複数で構成されている処理の場合はサブルーチンを分割して交番を1-1,1-2のように階層をとって振り直し、
    2-4. 振り直したサブルーチンに対して上記のステップを実施します。

3. 処理ロジックの記載
    2.で分割し、項番が付けられたサブルーチンごとに処理の概要を記載します。
    また各処理の詳細を記載します。テーブルアクセスを伴う処理と、ファイルの入出力を伴う処理についてはそのアクセス対象の項目についても記載します。
    また処理のあとにどの処理が呼ばれるか、後続の処理についても項番を記載します。

4. 処理フロー
    3. で記載した処理の順になるようにフローズを記載していきます。



