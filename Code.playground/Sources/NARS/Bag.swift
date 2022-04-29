import Dispatch

public final class Bag<I: Item> {
    var buckets: [[I]]
    var items: [String: I] = [:]
    
    internal let levels: Int
    internal let capacity: Int
    internal var currentLevel: Int = 0
    
    internal var queue = DispatchQueue(label: "ioqueue", qos: .background)
    
    public init(_ levels: Int = 10, _ capacity: Int = 100) {
        buckets = Array(repeating: [], count: levels)
        self.levels = levels
        self.capacity = capacity
    }
    
    @discardableResult
    public func put(_ item: I) -> I? {
        queue.sync {
            var item = item
            let oldItem = items[item.identifier]
            if let oldItem = oldItem {
                item.priority = max(oldItem.priority, item.priority)
                removeFromBucket(oldItem)
            }
            items[item.identifier] = item
            return addToBucket(item)
        }
    }
    
    public func get() -> I? {
        queue.sync {
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
    }
    
    public func get(_ identifier: String) -> I? {
        queue.sync {
            if let item = items[identifier] {
                removeFromBucket(item)
                items.removeValue(forKey: item.identifier)
                return item
            }
            return nil
        }
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
