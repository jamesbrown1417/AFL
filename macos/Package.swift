// swift-tools-version: 6.2

import PackageDescription

let package = Package(
    name: "AFLEdgeMac",
    platforms: [
        .macOS("26.0"),
    ],
    products: [
        .executable(
            name: "AFLEdgeMac",
            targets: ["AFLEdgeMac"]
        ),
    ],
    targets: [
        .executableTarget(
            name: "AFLEdgeMac",
            resources: [
                .process("Resources"),
            ]
        ),
        .testTarget(
            name: "AFLEdgeMacTests",
            dependencies: ["AFLEdgeMac"]
        ),
    ]
)
