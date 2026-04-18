import AppKit
import SwiftUI

@main
struct AFLEdgeMacApp: App {
    @NSApplicationDelegateAdaptor(AppDelegate.self) private var appDelegate
    @State private var container = AppContainer()

    init() {
        NSApplication.shared.appearance = NSAppearance(named: .aqua)
    }

    var body: some Scene {
        WindowGroup("AFL Edge", id: "main") {
            ContentView(container: container)
                .frame(minWidth: 1180, minHeight: 760)
                .tint(AFLTheme.accent)
                .preferredColorScheme(.light)
        }
        .commands {
            AFLEdgeCommands()
        }

        Settings {
            SettingsView(
                settingsStore: container.settingsStore,
                api: container.apiClient
            )
        }
    }
}

final class AppDelegate: NSObject, NSApplicationDelegate {
    func applicationDidFinishLaunching(_ notification: Notification) {
        NSApp.appearance = NSAppearance(named: .aqua)
        NSApp.setActivationPolicy(.regular)
        NSApp.activate(ignoringOtherApps: true)
        AppLog.app.info("AFL Edge macOS launched")
    }
}

struct AFLEdgeCommands: Commands {
    @FocusedValue(\.refreshAction) private var refreshAction
    @FocusedValue(\.toggleInspectorAction) private var toggleInspectorAction
    @FocusedValue(\.clearDraftAction) private var clearDraftAction
    @FocusedValue(\.focusSearchAction) private var focusSearchAction

    var body: some Commands {
        CommandMenu("AFL Edge") {
            Button("Refresh") {
                refreshAction?()
            }
            .keyboardShortcut("r")
            .disabled(refreshAction == nil)

            Button("Focus Search") {
                focusSearchAction?()
            }
            .keyboardShortcut("f")
            .disabled(focusSearchAction == nil)

            Button("Toggle Inspector") {
                toggleInspectorAction?()
            }
            .keyboardShortcut("i", modifiers: [.command, .option])
            .disabled(toggleInspectorAction == nil)

            Divider()

            Button("Clear Current Draft") {
                clearDraftAction?()
            }
            .keyboardShortcut(.delete, modifiers: [.command])
            .disabled(clearDraftAction == nil)
        }
    }
}

struct RefreshActionKey: FocusedValueKey {
    typealias Value = () -> Void
}

struct ToggleInspectorActionKey: FocusedValueKey {
    typealias Value = () -> Void
}

struct ClearDraftActionKey: FocusedValueKey {
    typealias Value = () -> Void
}

struct FocusSearchActionKey: FocusedValueKey {
    typealias Value = () -> Void
}

extension FocusedValues {
    var refreshAction: (() -> Void)? {
        get { self[RefreshActionKey.self] }
        set { self[RefreshActionKey.self] = newValue }
    }

    var toggleInspectorAction: (() -> Void)? {
        get { self[ToggleInspectorActionKey.self] }
        set { self[ToggleInspectorActionKey.self] = newValue }
    }

    var clearDraftAction: (() -> Void)? {
        get { self[ClearDraftActionKey.self] }
        set { self[ClearDraftActionKey.self] = newValue }
    }

    var focusSearchAction: (() -> Void)? {
        get { self[FocusSearchActionKey.self] }
        set { self[FocusSearchActionKey.self] = newValue }
    }
}
