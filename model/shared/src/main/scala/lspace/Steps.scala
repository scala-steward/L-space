package lspace

sealed trait Step
sealed trait Terminate            extends Step
sealed trait FilterStep           extends Step
sealed trait HasStep              extends FilterStep
sealed trait GlobalFilterStep     extends FilterStep
sealed trait EnvironmentStep      extends Step
sealed trait TraverseStep         extends Step
sealed trait LabelStep            extends Step
sealed trait ProjectionStep       extends Step
sealed trait MapStep              extends ProjectionStep with GroupingStep
sealed trait ResourceStep         extends Step
sealed trait GraphStep            extends ResourceStep
sealed trait BranchStep           extends Step
sealed trait ClipStep             extends FilterStep
sealed trait GroupingStep         extends Step
sealed trait BarrierStep          extends Step
sealed trait GroupingBarrierStep  extends BarrierStep with GroupingStep //with TraverseStep
sealed trait ReducingStep         extends Step
sealed trait ReducingBarrierStep  extends BarrierStep with ReducingStep //with TraverseStep
sealed trait FilterBarrierStep    extends BarrierStep with FilterStep   //with ReducingStep
sealed trait RearrangeStep        extends Step
sealed trait RearrangeBarrierStep extends RearrangeStep with BarrierStep

enum Has[key <: String] extends HasStep:
  private case HasKey[key <: String](
    key: key
  ) extends Has[key]
  private case HasKeyWithValue[key <: String, predicate <: P[_]](
    key: key,
    predicate: predicate
  ) extends Has[key]

  def apply[key <: String](
    key: key
  ): Has[key.type] = HasKey(key)
  def apply[key <: String, predicate <: P[_]](
    key: key,
    predicate: predicate
  ): Has[key.type] = HasKeyWithValue(key, predicate)

  type HasType[X] = X match {
    case X => X
  }

enum MoveStep[key <: String] extends BranchStep:
  private case In[key <: String](key: key) extends MoveStep[key]
  private case Out[key <: String](key: key) extends MoveStep[key]

  def apply[key <: String](key: key): In[key.type] = In(key)
  def apply[key <: String](key: key): Out[key.type] = Out(key)
