
public func allCases<Root>(for: Root.Type) -> [Root]? {
    guard
      let metadata = EnumMetadata(Root.self),
      metadata.kind == .enumeration,
      metadata.typeDescriptor.fieldDescriptor != nil
    else {
      return nil
    }

    let typeDescriptor = metadata.typeDescriptor
    let payloadCaseCount = typeDescriptor.payloadCaseCount
    guard payloadCaseCount == 0 else { return nil }
    let emptyCaseCount = typeDescriptor.emptyCaseCount
    return (0..<emptyCaseCount).map { withInjectedTag(tag: $0) }
}
    private func withInjectedTag<Enum>(
      tag: UInt32
    ) -> Enum {
        var root = UnsafeMutablePointer<Enum>.allocate(capacity: 1).pointee
        withUnsafeMutableBytes(of: &root) { rawBuffer in
        let pointer = rawBuffer.baseAddress!
        let metadata = EnumMetadata(assumingEnum: Enum.self)
        metadata.destructivelyInjectTag(tag, intoPayload: pointer)
      }
      return root
    }

private protocol Metadata {
  var ptr: UnsafeRawPointer { get }
}

extension Metadata {
  var valueWitnessTable: ValueWitnessTable {
    ValueWitnessTable(
      ptr: self.ptr.load(fromByteOffset: -pointerSize, as: UnsafeRawPointer.self)
    )
  }

  var kind: MetadataKind { self.ptr.load(as: MetadataKind.self) }
}

private struct MetadataKind: Equatable {
  var rawValue: UInt

  // https://github.com/apple/swift/blob/main/include/swift/ABI/MetadataValues.h
  // https://github.com/apple/swift/blob/main/include/swift/ABI/MetadataKind.def
  static var enumeration: Self { .init(rawValue: 0x201) }
  static var optional: Self { .init(rawValue: 0x202) }
  static var tuple: Self { .init(rawValue: 0x301) }
  static var existential: Self { .init(rawValue: 0x303) }
}

private struct EnumMetadata: Metadata {
  let ptr: UnsafeRawPointer

  init(assumingEnum type: Any.Type) {
    self.ptr = unsafeBitCast(type, to: UnsafeRawPointer.self)
  }

  init?(_ type: Any.Type) {
    self.init(assumingEnum: type)
    guard self.kind == .enumeration || self.kind == .optional else { return nil }
  }

  var genericArguments: GenericArgumentVector? {
    guard typeDescriptor.flags.contains(.isGeneric) else { return nil }
    return .init(ptr: self.ptr.advanced(by: 2 * pointerSize))
  }

  var typeDescriptor: EnumTypeDescriptor {
    EnumTypeDescriptor(
      ptr: self.ptr.load(fromByteOffset: pointerSize, as: UnsafeRawPointer.self)
    )
  }

  func tag<Enum>(of value: Enum) -> UInt32 {
    withUnsafePointer(to: value) {
      self.valueWitnessTable.getEnumTag($0, self.ptr)
    }
  }
}

extension EnumMetadata {
  func associatedValueType(forTag tag: UInt32) -> Any.Type {
    guard
      let typeName = self.typeDescriptor.fieldDescriptor?.field(atIndex: tag).typeName,
      let type = swift_getTypeByMangledNameInContext(
        typeName.ptr, typeName.length,
        genericContext: self.typeDescriptor.ptr,
        genericArguments: self.genericArguments?.ptr
      )
    else {
      return Void.self
    }

    return type
  }
}

@_silgen_name("swift_getTypeByMangledNameInContext")
private func swift_getTypeByMangledNameInContext(
  _ name: UnsafePointer<UInt8>,
  _ nameLength: UInt,
  genericContext: UnsafeRawPointer?,
  genericArguments: UnsafeRawPointer?
)
  -> Any.Type?

extension EnumMetadata {
  func destructivelyProjectPayload(of value: UnsafeMutableRawPointer) {
    self.valueWitnessTable.destructiveProjectEnumData(value, ptr)
  }

  func destructivelyInjectTag(_ tag: UInt32, intoPayload payload: UnsafeMutableRawPointer) {
    self.valueWitnessTable.destructiveInjectEnumData(payload, tag, ptr)
  }
}

private struct EnumTypeDescriptor: Equatable {
  let ptr: UnsafeRawPointer

  var flags: Flags { Flags(rawValue: self.ptr.load(as: UInt32.self)) }

  var fieldDescriptor: FieldDescriptor? {
    self.ptr
      .advanced(by: 4 * 4)
      .loadRelativePointer()
      .map(FieldDescriptor.init)
  }

  var payloadCaseCount: UInt32 { self.ptr.load(fromByteOffset: 5 * 4, as: UInt32.self) & 0xFFFFFF }

  var emptyCaseCount: UInt32 { self.ptr.load(fromByteOffset: 6 * 4, as: UInt32.self) }
}

extension EnumTypeDescriptor {
  struct Flags: OptionSet {
    let rawValue: UInt32

    static var isGeneric: Self { .init(rawValue: 0x80) }
  }
}

private struct TupleMetadata: Metadata {
  let ptr: UnsafeRawPointer

  init?(_ type: Any.Type) {
    self.ptr = unsafeBitCast(type, to: UnsafeRawPointer.self)
    guard self.kind == .tuple else { return nil }
  }

  var elementCount: UInt {
    self.ptr
      .advanced(by: pointerSize)  // kind
      .load(as: UInt.self)
  }

  var labels: UnsafePointer<UInt8>? {
    self.ptr
      .advanced(by: pointerSize)  // kind
      .advanced(by: pointerSize)  // elementCount
      .load(as: UnsafePointer<UInt8>?.self)
  }

  func element(at i: Int) -> Element {
    Element(
      ptr:
        self.ptr
        .advanced(by: pointerSize)  // kind
        .advanced(by: pointerSize)  // elementCount
        .advanced(by: pointerSize)  // labels pointer
        .advanced(by: i * 2 * pointerSize)
    )
  }
}

extension TupleMetadata {
  struct Element: Equatable {
    let ptr: UnsafeRawPointer

    var type: Any.Type { self.ptr.load(as: Any.Type.self) }

    var offset: UInt { self.ptr.load(fromByteOffset: pointerSize, as: UInt.self) }

    static func == (lhs: Element, rhs: Element) -> Bool {
      lhs.type == rhs.type && lhs.offset == rhs.offset
    }
  }
}

extension TupleMetadata {
  func hasSameLayout(as other: TupleMetadata) -> Bool {
    self.elementCount == other.elementCount
      && (0..<Int(self.elementCount)).allSatisfy { self.element(at: $0) == other.element(at: $0) }
  }
}

private struct ExistentialMetadata: Metadata {
  let ptr: UnsafeRawPointer

  init?(_ type: Any.Type?) {
    self.ptr = unsafeBitCast(type, to: UnsafeRawPointer.self)
    guard self.kind == .existential else { return nil }
  }
}

private struct FieldDescriptor {
  let ptr: UnsafeRawPointer

  /// The size of a FieldRecord as stored in the executable.
  var recordSize: Int { Int(self.ptr.advanced(by: 2 * 4 + 2).load(as: UInt16.self)) }

  func field(atIndex i: UInt32) -> FieldRecord {
    FieldRecord(
      ptr: self.ptr.advanced(by: 2 * 4 + 2 * 2 + 4).advanced(by: Int(i) * recordSize)
    )
  }
}

private struct FieldRecord {
  let ptr: UnsafeRawPointer

  var flags: Flags { Flags(rawValue: self.ptr.load(as: UInt32.self)) }

  var typeName: MangledTypeName? {
    self.ptr
      .advanced(by: 4)
      .loadRelativePointer()
      .map { MangledTypeName(ptr: $0.assumingMemoryBound(to: UInt8.self)) }
  }
}

extension FieldRecord {
  struct Flags: OptionSet {
    var rawValue: UInt32

    static var isIndirectCase: Self { .init(rawValue: 1) }
  }
}

private struct MangledTypeName {
  let ptr: UnsafePointer<UInt8>

  var length: UInt {
    // https://github.com/apple/swift/blob/main/docs/ABI/Mangling.rst
    var ptr = self.ptr
    while true {
      switch ptr.pointee {
      case 0:
        return UInt(bitPattern: ptr - self.ptr)
      case 0x01...0x17:
        // Relative symbolic reference
        ptr = ptr.advanced(by: 5)
      case 0x18...0x1f:
        // Absolute symbolic reference
        ptr = ptr.advanced(by: 1 + pointerSize)
      default:
        ptr = ptr.advanced(by: 1)
      }
    }
  }
}

private struct ValueWitnessTable {
  let ptr: UnsafeRawPointer

  var getEnumTag: @convention(c) (_ value: UnsafeRawPointer, _ metadata: UnsafeRawPointer) -> UInt32
  {
    self.ptr.advanced(by: 10 * pointerSize + 2 * 4).loadInferredType()
  }

  // This witness transforms an enum value into its associated value, in place.
  var destructiveProjectEnumData:
    @convention(c) (_ value: UnsafeMutableRawPointer, _ metadata: UnsafeRawPointer) -> Void
  {
    self.ptr.advanced(by: 11 * pointerSize + 2 * 4).loadInferredType()
  }

  // This witness transforms an associated value into its enum value, in place.
  var destructiveInjectEnumData:
    @convention(c) (_ value: UnsafeMutableRawPointer, _ tag: UInt32, _ metadata: UnsafeRawPointer)
      -> Void
  {
    self.ptr.advanced(by: 12 * pointerSize + 2 * 4).loadInferredType()
  }
}

private struct GenericArgumentVector {
  let ptr: UnsafeRawPointer
}

extension GenericArgumentVector {
  func type(atIndex i: Int) -> Any.Type {
    return ptr.load(fromByteOffset: i * pointerSize, as: Any.Type.self)
  }
}

extension UnsafeRawPointer {
  fileprivate func loadInferredType<Type>() -> Type {
    self.load(as: Type.self)
  }

  fileprivate func loadRelativePointer() -> UnsafeRawPointer? {
    let offset = Int(load(as: Int32.self))
    return offset == 0 ? nil : self + offset
  }
}

// This is the size of any Unsafe*Pointer and also the size of Int and UInt.
private let pointerSize = MemoryLayout<UnsafeRawPointer>.size
