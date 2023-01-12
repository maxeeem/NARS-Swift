// swift-tools-version:5.6
import PackageDescription
let package = Package(
    name: "TokamakApp",
    platforms: [.macOS(.v11), .iOS(.v13)],
    products: [
        .executable(name: "TokamakApp", targets: ["TokamakApp"])
    ],
    dependencies: [
        .package(url: "https://github.com/TokamakUI/Tokamak", from: "0.11.0")
    ],
    targets: [
        .executableTarget(
            name: "TokamakApp",
            dependencies: [
                .product(name: "TokamakShim", package: "Tokamak")
            ]),
        .testTarget(
            name: "TokamakAppTests",
            dependencies: ["TokamakApp"]),
    ]
)
