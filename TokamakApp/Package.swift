// swift-tools-version:5.6
import PackageDescription
let package = Package(
    name: "TokamakApp",
    platforms: [.macOS(.v11), .iOS(.v13)],
    products: [
        .executable(name: "TokamakApp", targets: ["TokamakApp"])
    ],
    dependencies: [
        .package(url: "https://github.com/TokamakUI/Tokamak", from: "0.11.0"),
        .package(path: "../../NARS-Swift")
    ],
    targets: [
        .executableTarget(
            name: "TokamakApp",
            dependencies: [
                .product(name: "TokamakShim", package: "Tokamak"),
                .product(name: "NARS", package: "NARS-Swift"),
                .product(name: "Narsese", package: "NARS-Swift")
            ],
            linkerSettings: [
                .unsafeFlags(
                    ["-Xlinker", "-z", "-Xlinker", "stack-size=131072"]//16777216
                )
            ]
        ),
        .testTarget(
            name: "TokamakAppTests",
            dependencies: ["TokamakApp"]),
    ]
)
