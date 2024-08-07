public protocol AbstractBag {
    associatedtype I: Item
    
    var items: [String: I] { get }

    @discardableResult
    func put(_ item: I) -> I?
    func get() -> I?
    func get(_ identifier: String) -> I?
    func peek() -> I?
    func peek(_ identifier: String) -> I?
}

public final class Bag<I: Item>: AbstractBag {
    var buckets: [[I]]
    public var items: [String: I] = [:]
    
    internal let levels: Int
    internal let capacity: Int
    internal var currentLevel: Int
        
    public init(_ levels: Int = 100, _ capacity: Int = 10000) {
        buckets = Array(repeating: [], count: levels)
        self.levels = levels
        self.capacity = capacity
        self.currentLevel = levels - 1
    }
    
    @discardableResult
    public func put(_ item: I) -> I? {
        var item = item
        let oldItem = items[item.identifier]
        if let oldItem = oldItem {
            item.priority = max(oldItem.priority, item.priority)
            removeFromBucket(oldItem)
        }
        items[item.identifier] = item
        return addToBucket(item)
    }
    
    public func get() -> I? {
        if items.isEmpty {
            return nil
        }
        currentLevel = selectNonEmptyLevel()
        if buckets[currentLevel].isEmpty {
            return nil
        }
        let item = buckets[currentLevel].removeFirst()
        items.removeValue(forKey: item.identifier)
        return item
    }
    
    public func get(_ identifier: String) -> I? {
        if let item = items[identifier] {
            removeFromBucket(item)
            items.removeValue(forKey: item.identifier)
            return item
        }
        return nil
    }
    
    public func peek() -> I? {
        if items.isEmpty {
            return nil
        }
        currentLevel = selectNonEmptyLevel()
        return buckets[currentLevel].first
    }
    
    public func peek(_ identifier: String) -> I? {
        return items[identifier]
    }

    private func getLevel(_ item: I) -> Int {
        let fl = item.priority * Double(buckets.count)
        let level = Int(fl.rounded(.down) - 1)
        return max(level, 0)
    }
    
    // TODO: add probabilistic distributor from OpenNARS
    private func selectNonEmptyLevel() -> Int {
        var selectedLevel = currentLevel
        var cache = Array(0..<levels)
        while buckets[selectedLevel].isEmpty && cache.count > 0 {
            /// https://stackoverflow.com/a/27541537
            let randomKey = Int.random(in: 0..<cache.count)
            selectedLevel = cache[randomKey]
            cache.swapAt(randomKey, cache.count - 1)
            cache.removeLast()
        }
        return selectedLevel
    }
    
    private func addToBucket(_ item: I) -> I? {
        var oldItem: I?
        if items.count > capacity {
            var level = 0
            while buckets[level].isEmpty, level < levels {
                level += 1
            }
            oldItem = buckets[level].removeFirst()
            if let removed = oldItem {
                items.removeValue(forKey: removed.identifier)
            }
        }
        let level = getLevel(item)
        buckets[level].append(item)
        return oldItem
    }
    
    private func removeFromBucket(_ item: I) {
        let level = getLevel(item)
        var items = buckets[level]
        items.removeAll(where: { $0.identifier == item.identifier })
        buckets[level] = items
    }
}


// MARK: - WrappedBag

/// Read access to wrapped Bag with writes to internal bag
//public final class WrappedBag<I: Item>: AbstractBag {
//    weak var wrapped: Bag<I>?
//    var bag = Bag<I>()
//    
//    init(_ bag: Bag<I>) {
//        wrapped = bag
//    }
//    
//    public func reset() {
//        bag = Bag<I>()
//    }
//    
//    @discardableResult
//    public func put(_ item: I) -> I? {
//        if item != wrapped?.peek(item.identifier) {
//            bag.put(item) // items have diverged
//        }
//        return nil
//    }
//    
//    public func get() -> I? {
//        bag.get() ?? wrapped?.peek()
//    }
//    
//    public func get(_ identifier: String) -> I? {
//        bag.get(identifier) ?? wrapped?.peek(identifier)
//    }
//    
//    public func peek() -> I? {
//        bag.peek() ?? wrapped?.peek()
//    }
//    
//    public func peek(_ identifier: String) -> I? {
//        bag.peek(identifier) ?? wrapped?.peek(identifier)
//    }
//}
