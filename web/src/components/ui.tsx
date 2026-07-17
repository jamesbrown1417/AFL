import { forwardRef, useEffect, useRef, useSyncExternalStore } from 'react'
import type { ButtonHTMLAttributes, InputHTMLAttributes, ReactNode, RefObject, SelectHTMLAttributes } from 'react'
import { ArrowDown, ArrowDownUp, ArrowUp, X } from 'lucide-react'
import clsx from 'clsx'

export function Button({
  className,
  variant = 'primary',
  ...props
}: ButtonHTMLAttributes<HTMLButtonElement> & { variant?: 'primary' | 'secondary' | 'ghost' | 'danger' | 'accent' }) {
  return <button className={clsx('button', `button--${variant}`, className)} {...props} />
}

export function IconButton({ className, label, children, ...props }: ButtonHTMLAttributes<HTMLButtonElement> & { label: string }) {
  return (
    <button className={clsx('icon-button', className)} aria-label={label} title={label} {...props}>
      {children}
    </button>
  )
}

export function Panel({ className, children }: { className?: string; children: ReactNode }) {
  return <section className={clsx('panel', className)}>{children}</section>
}

export function Field({ label, children, className }: { label: string; children: ReactNode; className?: string }) {
  return (
    <label className={clsx('field', className)}>
      <span>{label}</span>
      {children}
    </label>
  )
}

export const TextInput = forwardRef<HTMLInputElement, InputHTMLAttributes<HTMLInputElement>>(function TextInput(props, ref) {
  return <input ref={ref} className="input" {...props} />
})

export function Select(props: SelectHTMLAttributes<HTMLSelectElement>) {
  return <select className="input select" {...props} />
}

export function Toggle({ checked, onChange, label }: { checked: boolean; onChange: (checked: boolean) => void; label: string }) {
  return (
    <label className="toggle">
      <input type="checkbox" checked={checked} onChange={(event) => onChange(event.currentTarget.checked)} />
      <span aria-hidden="true" />
      <b>{label}</b>
    </label>
  )
}

export function Segmented<T extends string>({
  value,
  options,
  onChange,
  ariaLabel,
  className,
}: {
  value: T
  options: { value: T; label: string }[]
  onChange: (value: T) => void
  ariaLabel: string
  className?: string
}) {
  return (
    <div className={clsx('segmented', className)} role="tablist" aria-label={ariaLabel}>
      {options.map((option) => (
        <button
          key={option.value}
          type="button"
          role="tab"
          aria-selected={value === option.value}
          className={clsx(value === option.value && 'is-active')}
          onClick={() => onChange(option.value)}
        >
          {option.label}
        </button>
      ))}
    </div>
  )
}

export function Chip({
  active,
  children,
  onClick,
}: {
  active?: boolean
  children: ReactNode
  onClick?: () => void
}) {
  return (
    <button type="button" className={clsx('chip', active && 'is-active')} aria-pressed={active} onClick={onClick}>
      {children}
    </button>
  )
}

export function EmptyState({ title, body, action }: { title: string; body: string; action?: ReactNode }) {
  return (
    <div className="empty-state">
      <h3>{title}</h3>
      <p>{body}</p>
      {action}
    </div>
  )
}

export function ErrorBanner({ message }: { message: string }) {
  return <div className="error-banner" role="alert">{message}</div>
}

export function AdaptiveRail({
  open,
  onClose,
  label,
  className,
  triggerRef,
  children,
}: {
  open: boolean
  onClose: () => void
  label: string
  className?: string
  triggerRef?: RefObject<HTMLElement | null>
  children: ReactNode
}) {
  const railRef = useRef<HTMLElement>(null)
  const overlayMode = useSyncExternalStore(
    (onStoreChange) => {
      const media = window.matchMedia('(max-width: 1700px)')
      media.addEventListener('change', onStoreChange)
      return () => media.removeEventListener('change', onStoreChange)
    },
    () => window.matchMedia('(max-width: 1700px)').matches,
    () => false,
  )

  useEffect(() => {
    if (!open || !overlayMode) return
    const previousFocus = document.activeElement instanceof HTMLElement ? document.activeElement : null
    const restoreTarget = triggerRef?.current ?? previousFocus
    const rail = railRef.current
    window.requestAnimationFrame(() => rail?.focus())

    const handleKeyDown = (event: KeyboardEvent) => {
      if (event.key === 'Escape') {
        event.preventDefault()
        onClose()
        return
      }
      if (event.key !== 'Tab' || !rail) return
      const focusable = Array.from(rail.querySelectorAll<HTMLElement>('button:not([disabled]), input:not([disabled]), select:not([disabled]), [tabindex]:not([tabindex="-1"])'))
      if (focusable.length === 0) return
      const first = focusable[0]
      const last = focusable[focusable.length - 1]
      if (event.shiftKey && document.activeElement === first) {
        event.preventDefault()
        last.focus()
      } else if (!event.shiftKey && document.activeElement === last) {
        event.preventDefault()
        first.focus()
      }
    }
    document.addEventListener('keydown', handleKeyDown)
    return () => {
      document.removeEventListener('keydown', handleKeyDown)
      window.requestAnimationFrame(() => restoreTarget?.focus())
    }
  }, [onClose, open, overlayMode, triggerRef])

  return (
    <>
      <div className={clsx('adaptive-rail-backdrop', open && 'is-open')} aria-hidden="true" onClick={onClose} />
      <aside
        ref={railRef}
        className={clsx('adaptive-rail', className, open && 'is-open')}
        aria-label={label}
        aria-hidden={overlayMode && !open ? true : undefined}
        inert={overlayMode && !open ? true : undefined}
        tabIndex={-1}
      >
        <IconButton className="adaptive-rail-close" label={`Close ${label}`} onClick={onClose}>
          <X size={18} />
        </IconButton>
        {children}
      </aside>
    </>
  )
}

export function StatPill({ label, value, tone = 'neutral' }: { label: string; value: string; tone?: 'neutral' | 'good' | 'warn' | 'bad' }) {
  return (
    <div className={clsx('stat-pill', `stat-pill--${tone}`)}>
      <span>{label}</span>
      <b>{value}</b>
    </div>
  )
}

export function SortIcon({ state }: { state: false | 'asc' | 'desc' }) {
  if (state === 'asc') return <ArrowUp size={12} />
  if (state === 'desc') return <ArrowDown size={12} />
  return <ArrowDownUp size={12} />
}

export function ConfirmDialog({
  message,
  onConfirm,
  onCancel,
}: {
  message: string
  onConfirm: () => void
  onCancel: () => void
}) {
  return (
    <div className="modal-overlay" onClick={onCancel}>
      <div className="modal confirm-dialog" role="dialog" aria-modal="true" aria-label="Confirm action" onClick={(event) => event.stopPropagation()}>
        <p className="confirm-dialog-message">{message}</p>
        <div className="confirm-dialog-actions">
          <Button variant="ghost" onClick={onCancel}>Cancel</Button>
          <Button variant="danger" onClick={onConfirm}>Confirm</Button>
        </div>
      </div>
    </div>
  )
}
