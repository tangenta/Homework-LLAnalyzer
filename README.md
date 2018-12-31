# LLAnalyzer——LL(1)分析器

目录结构：
```$xslt
.
├── build.sbt
├── LICENSE
├── project
│   ├── build.properties
│   └── META-INF
│       └── MANIFEST.MF
├── README.md
└── src
    ├── main
    │   ├── java
    │   │   ├── LLAnalyzer.form     # idea 窗体
    │   │   ├── LLAnalyzer.java     # 程序入口，按钮逻辑
    │   │   └── META-INF
    │   │       └── MANIFEST.MF
    │   └── scala
    │       └── analyzer
    │           ├── Adapter.scala   # scala和java的沟通桥梁
    │           ├── Algorithm.scala # first,follow,表的字符串化 模拟表解析
    │           ├── Basics.scala    # 基本数据结构
    │           ├── GrammarScanner.scala  # 文法扫描器 
    │           ├── LeftCommonFactorDetector.scala  # 左公因子的检测与消除
    │           ├── LeftRecursiveDetector.scala     # 左递归的检测与消除
    │           └── LLTable.scala   # LL(1)分析表
    └── test
        └── scala
            └── analyzer
                ├── AlgorithmTest.scala
                ├── GrammarScannerTest.scala
                ├── LeftCommonFactorDetectorTest.scala
                ├── LeftRecursiveDetectorTest.scala
                └── LLTableTest.scala

11 directories, 20 files

```