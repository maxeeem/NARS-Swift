// swift-tools-version:5.6
// The swift-tools-version declares the minimum version of Swift required to build this package.

// NOTE:
// when building for WASM, increase default stack size of 64KB
// swift build --triple wasm32-unknown-wasi -Xlinker -z -Xlinker stack-size=131072

import PackageDescription

let package = Package(
    name: "NARS-Swift",
    products: [
        // Products define the executables and libraries a package produces, and make them visible to other packages.
        .library(
            name: "NAL",
            targets: ["NAL"]),
        .library(
            name: "NARS",
            targets: ["NARS"]),
        .library(
            name: "Narsese",
            targets: ["Narsese"]),
        .executable(
            name: "nar",
            targets: ["nar"]),
        .executable(
            name: "View",
            targets: ["View"])
    ],
    dependencies: [
        // Dependencies declare other packages that this package depends on.
        // .package(url: /* package url */, from: "1.0.0"),
    ],
    targets: [
        // Targets are the basic building blocks of a package. A target can define a module or a test suite.
        // Targets can depend on other targets in this package, and on products in packages this package depends on.
        .target(
            name: "NAL",
            path: "Code.playground/Sources/NAL"),
        .target(
            name: "NARS",
            dependencies: ["NAL"],
            path: "Code.playground/Sources/NARS"),
        .target(
            name: "Narsese",
            dependencies: ["NAL"]),
        .executableTarget(
            name: "nar",
            dependencies: ["NARS", "Narsese"],
            resources: [.copy("Commander/LICENSE")]),
        .executableTarget(
            name: "View",
            dependencies: ["NAL"],
            resources: nil),
        
        .testTarget(
            name: "NARS-Tests",
            dependencies: ["NARS", "Narsese"]),
    ]
)
