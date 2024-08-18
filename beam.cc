/**
 * @file
 * @brief Functions related to ranged attacks.
**/

#include "AppHdr.h"

#include "beam.h"

#include <algorithm>
#include <cmath>
#include <cstdarg>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <iostream>
#include <set>

#include "act-iter.h"
#include "areas.h"
#include "attack.h"
#include "attitude-change.h"
#include "bloodspatter.h"
#include "chardump.h"
#include "cloud.h"
#include "colour.h"
#include "coordit.h"
#include "delay.h"
#include "directn.h"
#include "dungeon.h"
#include "english.h"
#include "exercise.h"
#include "fight.h"
#include "fineff.h"
#include "god-abil.h"
#include "god-conduct.h"
#include "god-item.h"
#include "god-passive.h" // passive_t::convert_orcs
#include "item-use.h"
#include "item-prop.h"
#include "items.h"
#include "killer-type.h"
#include "libutil.h"
#include "losglobal.h"
#include "los.h"
#include "message.h"
#include "mon-behv.h"
#include "mon-death.h"
#include "mon-explode.h"
#include "mon-place.h"
#include "mon-poly.h"
#include "mon-util.h"
#include "mutation.h"
#include "nearby-danger.h"
#include "options.h"
#include "player-stats.h"
#include "potion.h"
#include "prompt.h"
#include "ranged-attack.h"
#include "religion.h"
#include "shout.h"
#include "spl-book.h"
#include "spl-clouds.h"
#include "spl-damage.h"
#include "spl-goditem.h"
#include "spl-monench.h"
#include "spl-summoning.h"
#include "spl-transloc.h"
#include "spl-util.h"
#include "spl-zap.h"
#include "state.h"
#include "stepdown.h"
#include "stringutil.h"
#include "target.h"
#include "teleport.h"
#include "terrain.h"
#include "throw.h"
#ifdef USE_TILE
 #include "tilepick.h"
#endif
#include "tiles-build-specific.h"
#include "transform.h"
#include "traps.h"
#include "viewchar.h"
#include "view.h"
#include "xom.h"

// Helper functions (some of these should probably be public).
static void _ench_animation(int flavour, const monster* mon = nullptr,
                            bool force = false);
static beam_type _chaos_beam_flavour(bolt* beam);
static string _beam_type_name(beam_type type);
int _ench_pow_to_dur(int pow);

tracer_info::tracer_info()
{
    reset();
}

void tracer_info::reset()
{
    count = power = hurt = helped = 0;
    dont_stop = false;
}

const tracer_info& tracer_info::operator+=(const tracer_info &other)
{
    count  += other.count;
    power  += other.power;
    hurt   += other.hurt;
    helped += other.helped;

    dont_stop = dont_stop || other.dont_stop;

    return *this;
}

bolt::bolt() : animate(bool(Options.use_animations & UA_BEAM)) {}

bool bolt::is_blockable() const
{
    // BEAM_ELECTRICITY is added here because chain lightning is not
    // a true beam (stops at the first target it gets to and redirects
    // from there)... but we don't want it shield blockable.
    return !pierce && !is_explosion && flavour != BEAM_ELECTRICITY
           && hit != AUTOMATIC_HIT && flavour != BEAM_VISUAL;
}

/// Can 'omnireflection' (from the Warlock's Mirror) potentially reflect this?
bool bolt::is_omnireflectable() const
{
    return !is_explosion && flavour != BEAM_VISUAL
            && origin_spell != SPELL_GLACIATE
            && flavour != BEAM_VAMPIRIC_DRAINING; // buggy :(
}

void bolt::emit_message(const char* m)
{
    const string message = m;
    if (!message_cache.count(message))
        mpr(m);

    message_cache.insert(message);
}

kill_category bolt::whose_kill() const
{
    if (YOU_KILL(thrower) || source_id == MID_YOU_FAULTLESS)
        return KC_YOU;
    else if (MON_KILL(thrower))
    {
        if (source_id == MID_ANON_FRIEND)
            return KC_FRIENDLY;
        const monster* mon = monster_by_mid(source_id);
        if (mon && mon->friendly())
            return KC_FRIENDLY;
    }
    return KC_OTHER;
}

// A simple animated flash from Rupert Smith (expanded to be more
// generic).
static void _zap_animation(int colour, const monster* mon = nullptr,
                           bool force = false)
{
    coord_def p = you.pos();

    if (mon)
    {
        if (!force && !mon->visible_to(&you))
            return;

        p = mon->pos();
    }

    if (!you.see_cell(p))
        return;

    const coord_def drawp = grid2view(p);

    if (in_los_bounds_v(drawp))
    {
#ifdef USE_TILE
        view_add_tile_overlay(p, tileidx_zap(colour));
#endif
        view_add_glyph_overlay(p, {dchar_glyph(DCHAR_FIRED_ZAP),
                                   static_cast<unsigned short>(colour)});
        animation_delay(50, true);
    }
}

// Special front function for zap_animation to interpret enchantment flavours.
static void _ench_animation(int flavour, const monster* mon, bool force)
{
    element_type elem;
    switch (flavour)
    {
    case BEAM_HEALING:
        elem = ETC_HEAL;
        break;
    case BEAM_INFESTATION:
    case BEAM_PAIN:
    case BEAM_AGONY:
    case BEAM_CURSE_OF_AGONY:
    case BEAM_VILE_CLUTCH:
    case BEAM_VAMPIRIC_DRAINING:
    case BEAM_SOUL_SPLINTER:
        elem = ETC_UNHOLY;
        break;
    case BEAM_DISPEL_UNDEAD:
        elem = ETC_HOLY;
        break;
    case BEAM_POLYMORPH:
    case BEAM_MALMUTATE:
        elem = ETC_MUTAGENIC;
        break;
    case BEAM_CHAOS:
        elem = ETC_RANDOM;
        break;
    case BEAM_TELEPORT:
    case BEAM_BANISH:
    case BEAM_BLINK:
    case BEAM_BLINK_CLOSE:
    case BEAM_BECKONING:
        elem = ETC_WARP;
        break;
    case BEAM_MAGIC:
        elem = ETC_MAGIC;
        break;
    case BEAM_ROOTS:
        elem = ETC_EARTH;
        break;
    default:
        elem = ETC_ENCHANT;
        break;
    }

    _zap_animation(element_colour(elem), mon, force);
}

// If needs_tracer is true, we need to check the beam path for friendly
// monsters.
spret zapping(zap_type ztype, int power, bolt &pbolt,
                   bool needs_tracer, const char* msg, bool fail)
{
    dprf(DIAG_BEAM, "zapping: power=%d", power);

    pbolt.thrower = KILL_YOU_MISSILE;

    // Check whether tracer goes through friendlies.
    // NOTE: Whenever zapping() is called with a randomised value for power
    // (or effect), player_tracer should be called directly with the highest
    // power possible respecting current skill, experience level, etc.
    if (needs_tracer && !player_tracer(ztype, power, pbolt))
        return spret::abort;

    fail_check();
    // Fill in the bolt structure.
    zappy(ztype, power, false, pbolt);

    if (msg)
        mpr(msg);

    if (ztype == ZAP_DIG)
        pbolt.aimed_at_spot = false;

    pbolt.fire();

    return spret::success;
}

// Returns true if the path is considered "safe", and false if there are
// monsters in the way the player doesn't want to hit.
bool player_tracer(zap_type ztype, int power, bolt &pbolt, int range)
{
    // Non-controlleable during confusion.
    // (We'll shoot in a different direction anyway.)
    if (you.confused())
        return true;

    zappy(ztype, power, false, pbolt);

    pbolt.is_tracer     = true;
    pbolt.source        = you.pos();
    pbolt.source_id     = MID_PLAYER;
    pbolt.attitude      = ATT_FRIENDLY;
    pbolt.thrower       = KILL_YOU_MISSILE;
    pbolt.overshoot_prompt = false;
    pbolt.passed_target = false;

    // Init tracer variables.
    pbolt.friend_info.reset();
    pbolt.foe_info.reset();

    pbolt.foe_ratio        = 100;
    pbolt.beam_cancelled   = false;
    pbolt.dont_stop_player = false;

    // Clear misc
    pbolt.seen          = false;
    pbolt.heard         = false;
    pbolt.reflections   = 0;
    pbolt.bounces       = 0;
    pbolt.loudness      = 0;

    // Save range before overriding it
    const int old_range = pbolt.range;
    if (range)
        pbolt.range = range;

    pbolt.fire();

    if (range)
        pbolt.range = old_range;

    // Should only happen if the player answered 'n' to one of those
    // "Fire through friendly?" prompts.
    if (pbolt.beam_cancelled)
    {
        dprf(DIAG_BEAM, "Beam cancelled.");
        you.turn_is_over = false;
        return false;
    }

    if (pbolt.friendly_past_target)
        pbolt.aimed_at_spot = true;

    // Set to non-tracing for actual firing.
    pbolt.is_tracer = false;
    return true;
}

// Returns true if the player wants / needs to abort based on god displeasure
// with targeting this target with this spell. Returns false otherwise.
static bool _stop_because_god_hates_target_prompt(monster* mon,
                                                  spell_type spell)
{
    if (spell == SPELL_TUKIMAS_DANCE)
    {
        const item_def * const first = mon->weapon(0);
        const item_def * const second = mon->weapon(1);
        bool prompt = first && god_hates_item(*first)
                      || second && god_hates_item(*second);
        if (prompt
            && !yesno("Animating this weapon would place you under penance. "
            "Really cast this spell?", false, 'n'))
        {
            return true;
        }
    }

    return false;
}

template<typename T>
class power_deducer
{
public:
    virtual T operator()(int pow, bool random = true) const = 0;
    virtual ~power_deducer() {}
};

typedef power_deducer<int> tohit_deducer;

template<int adder, int mult_num = 0, int mult_denom = 1>
class tohit_calculator : public tohit_deducer
{
public:
    int operator()(int pow, bool /*random*/) const override
    {
        return adder + pow * mult_num / mult_denom;
    }
};

typedef power_deducer<dice_def> dam_deducer;

template<int numdice, int adder, int mult_num, int mult_denom>
class dicedef_calculator : public dam_deducer
{
public:
    dice_def operator()(int pow, bool /*random*/) const override
    {
        return dice_def(numdice, adder + pow * mult_num / mult_denom);
    }
};

template<int numdice, int adder, int mult_num, int mult_denom>
class calcdice_calculator : public dam_deducer
{
public:
    dice_def operator()(int pow, bool random) const override
    {
        return calc_dice(numdice, adder + pow * mult_num / mult_denom, random);
    }
};

struct zap_info
{
    zap_type ztype;
    const char* name;           // nullptr means handled specially
    int player_power_cap;
    dam_deducer* player_damage;
    tohit_deducer* player_tohit;    // Enchantments have power modifier here
    dam_deducer* monster_damage;
    tohit_deducer* monster_tohit;
    colour_t colour;
    bool is_enchantment;
    beam_type flavour;
    dungeon_char_type glyph;
    bool can_beam;
    bool is_explosion;
};

#include "zap-data.h"

static int zap_index[NUM_ZAPS];

void init_zap_index()
{
    for (int i = 0; i < NUM_ZAPS; ++i)
        zap_index[i] = -1;

    for (unsigned int i = 0; i < ARRAYSZ(zap_data); ++i)
        zap_index[zap_data[i].ztype] = i;
}

static const zap_info* _seek_zap(zap_type z_type)
{
    ASSERT_RANGE(z_type, 0, NUM_ZAPS);
    if (zap_index[z_type] == -1)
        return nullptr;
    else
        return &zap_data[zap_index[z_type]];
}

bool zap_explodes(zap_type z_type)
{
    const zap_info* zinfo = _seek_zap(z_type);
    return zinfo && zinfo->is_explosion;
}

bool zap_is_enchantment(zap_type z_type)
{
    const zap_info* zinfo = _seek_zap(z_type);
    return zinfo && zinfo->is_enchantment;
}

int zap_to_hit(zap_type z_type, int power, bool is_monster)
{
    const zap_info* zinfo = _seek_zap(z_type);
    if (!zinfo)
        return 0;
    const tohit_deducer* hit_calc = is_monster ? zinfo->monster_tohit
                                               : zinfo->player_tohit;
    if (zinfo->is_enchantment)
        return 0;
    ASSERT(hit_calc);
    const int hit = (*hit_calc)(power);
    if (hit != AUTOMATIC_HIT && !is_monster && crawl_state.need_save)
        return max(0, hit - you.inaccuracy_penalty());
    return hit;
}

dice_def zap_damage(zap_type z_type, int power, bool is_monster, bool random)
{
    const zap_info* zinfo = _seek_zap(z_type);
    if (!zinfo)
        return dice_def(0,0);
    const dam_deducer* dam_calc = is_monster ? zinfo->monster_damage
                                             : zinfo->player_damage;
    return dam_calc ? (*dam_calc)(power, random) : dice_def(0,0);
}

colour_t zap_colour(zap_type z_type)
{
    const zap_info* zinfo = _seek_zap(z_type);
    if (!zinfo)
        return BLACK;
    return zinfo->colour;
}

int zap_power_cap(zap_type z_type)
{
    const zap_info* zinfo = _seek_zap(z_type);

    return zinfo ? zinfo->player_power_cap : 0;
}

int zap_ench_power(zap_type z_type, int pow, bool is_monster)
{
    const zap_info* zinfo = _seek_zap(z_type);
    if (!zinfo)
        return pow;

    if (zinfo->player_power_cap > 0 && !is_monster)
        pow = min(zinfo->player_power_cap, pow);

    tohit_deducer* ench_calc = is_monster ? zinfo->monster_tohit
                                          : zinfo->player_tohit;
    if (zinfo->is_enchantment && ench_calc)
        return (*ench_calc)(pow);
    else
        return pow;
}

static int _zap_loudness(zap_type zap, spell_type spell)
{
    const zap_info* zinfo = _seek_zap(zap);
    const int noise = spell_effect_noise(spell);
    const spell_flags flags = get_spell_flags(spell);

    // Explosions have noise handled separately.
    if (zinfo->is_explosion || testbits(flags, spflag::silent))
        return 0;
    else if (noise > 0)
        return noise;
    // Enchantments are (usually) silent.
    else if (zinfo->is_enchantment)
        return 0;
    else if (spell != SPELL_NO_SPELL)
        return spell_difficulty(spell);

    return 0;
}

#ifdef WIZARD
static bool _needs_monster_zap(const zap_info &zinfo)
{
    // for enchantments we can't know from this data
    if (zinfo.is_enchantment)
        return false;
    // if player tohit is missing from a non-enchantment, then it has to use
    // a fake monster cast
    if (!zinfo.player_tohit)
        return true;

    // otherwise, it should be possible to use a player zap (damage may or may
    // not be defined)
    return false;
}
#endif

void zappy(zap_type z_type, int power, bool is_monster, bolt &pbolt)
{
    const zap_info* zinfo = _seek_zap(z_type);

    // None found?
    if (zinfo == nullptr)
    {
        dprf("Couldn't find zap type %d", z_type);
        return;
    }

#ifdef WIZARD
    // we are in a wizmode cast scenario: use monster zap data to avoid crashes.
    // N.b. this suppresses some player effects, such as inaccuracy.
    if (!is_monster && you.wizard && _needs_monster_zap(*zinfo))
        is_monster = true;
#endif

    // Fill
    pbolt.name           = zinfo->name;
    pbolt.flavour        = zinfo->flavour;
    pbolt.real_flavour   = zinfo->flavour;
    pbolt.colour         = zinfo->colour;
    pbolt.glyph          = dchar_glyph(zinfo->glyph);
    pbolt.pierce         = zinfo->can_beam;
    pbolt.is_explosion   = zinfo->is_explosion;

    if (zinfo->player_power_cap > 0 && !is_monster)
        power = min(zinfo->player_power_cap, power);

    ASSERT(zinfo->is_enchantment == pbolt.is_enchantment());

    pbolt.ench_power = zap_ench_power(z_type, power, is_monster);

    if (zinfo->is_enchantment)
        pbolt.hit = AUTOMATIC_HIT;
    else
        pbolt.hit = zap_to_hit(z_type, power, is_monster);

    pbolt.damage = zap_damage(z_type, power, is_monster);

    if (pbolt.origin_spell == SPELL_NO_SPELL)
        pbolt.origin_spell = zap_to_spell(z_type);

    if (pbolt.loudness == 0)
        pbolt.loudness = _zap_loudness(z_type, pbolt.origin_spell);
}

bool bolt::can_affect_actor(const actor *act) const
{
    // Blinkbolt doesn't hit its caster, since they are the bolt.
    if (origin_spell == SPELL_BLINKBOLT && act->mid == source_id)
        return false;
    // Damnation doesn't blast the one firing.
    else if (item
            && item->props.exists(DAMNATION_BOLT_KEY)
            && act->mid == source_id)
    {
        return false;
    }
    // Xak'krixis' prisms are smart enough not to affect friendlies
    else if (origin_spell == SPELL_FULMINANT_PRISM && thrower == KILL_MON
        && act->temp_attitude() == attitude)
    {
        return false;
    }
    auto cnt = hit_count.find(act->mid);
    if (cnt != hit_count.end() && cnt->second >= 2)
    {
        // Note: this is done for balance, even if it hurts realism a bit.
        // It is arcane knowledge which wall patterns will cause lightning
        // to bounce thrice, double damage for ordinary bounces is enough.
#ifdef DEBUG_DIAGNOSTICS
        if (!quiet_debug)
            dprf(DIAG_BEAM, "skipping beam hit, affected them twice already");
#endif
        return false;
    }

    return true;
}

bool bolt::is_big_cloud() const
{
    return testbits(get_spell_flags(origin_spell), spflag::cloud);
}

coord_def bolt::leg_source() const
{
    if (bounces > 0 && map_bounds(bounce_pos))
        return bounce_pos;
    else
        return source;
}

// Reflect a beam back the direction it came. This is used
// by shields of reflection.
void bolt::reflect()
{
    reflections++;

    target = leg_source();
    source = pos();

    // Reset bounce_pos, so that if we somehow reflect again before reaching
    // the wall that we won't keep heading towards the wall.
    bounce_pos.reset();

    if (pos() == you.pos())
    {
        reflector = MID_PLAYER;
        count_action(CACT_BLOCK, -1, BLOCK_REFLECT);
    }
    else if (monster* m = monster_at(pos()))
        reflector = m->mid;
    else
    {
        reflector = MID_NOBODY;
#ifdef DEBUG
        dprf(DIAG_BEAM, "Bolt reflected by neither player nor "
             "monster (bolt = %s, item = %s)", name.c_str(),
             item ? item->name(DESC_PLAIN).c_str() : "none");
#endif
    }

    flavour = real_flavour;
    choose_ray();
}

void bolt::tracer_affect_player()
{
    if (flavour == BEAM_UNRAVELLING && player_is_debuffable())
        is_explosion = true;

    const actor* ag = agent();

    // Check whether thrower can see player, unless thrower == player.
    if (YOU_KILL(thrower))
    {
        if (!dont_stop_player && !harmless_to_player())
        {
            string prompt = make_stringf("That %s is likely to hit you. Continue anyway?",
                                         item ? name.c_str() : "beam");

            if (yesno(prompt.c_str(), false, 'n'))
            {
                friend_info.count++;
                friend_info.power += you.experience_level;
                // Don't ask about aiming at ourself twice.
                dont_stop_player = true;
            }
            else
            {
                canned_msg(MSG_OK);
                beam_cancelled = true;
                finish_beam();
            }
        }
    }
    else if (can_see_invis
             || !you.invisible()
             || ag && ag->as_monster()->friendly()
             || fuzz_invis_tracer())
    {
        if (mons_att_wont_attack(attitude) && !harmless_to_player())
        {
            friend_info.count++;
            friend_info.power += you.experience_level;
        }
        else
        {
            foe_info.count++;
            foe_info.power += you.experience_level;
        }
    }

    extra_range_used += range_used_on_hit();
}

int bolt::apply_lighting(int base_hit, const actor &targ) const
{
    if (targ.invisible() && !can_see_invis)
        base_hit /= 2;

    // We multiply these lighting effects by 2, since normally they're applied post-roll
    // where the effect (on average) counts doubled

    if (targ.backlit(false))
        base_hit += BACKLIGHT_TO_HIT_BONUS * 2;

    // Malus is already negative so must still be ADDED to the base_hit
    if (!nightvision && targ.umbra())
        base_hit += UMBRA_TO_HIT_MALUS * 2;

    return base_hit;
}

/* Determine whether the beam hit or missed the player, and tell them if it
 * missed.
 *
 * @return  true if the beam missed, false if the beam hit the player.
 */
bool bolt::misses_player()
{
    if (flavour == BEAM_VISUAL)
        return true;

    if ((is_explosion || aimed_at_feet)
        && origin_spell != SPELL_CALL_DOWN_LIGHTNING
        && origin_spell != SPELL_MOMENTUM_STRIKE)
    {
        return false;
    }

    int dodge = you.evasion();
    int real_tohit  = hit;

    if (real_tohit != AUTOMATIC_HIT)
        real_tohit = apply_lighting(real_tohit, you);

    const int SH = player_shield_class();
    if ((player_omnireflects() && is_omnireflectable()
         || is_blockable())
        && you.shielded()
        && !you.shield_exhausted()
        && !aimed_at_feet
        && (SH > 0 || you.duration[DUR_DIVINE_SHIELD]))
    {
        bool blocked = false;
        if (hit == AUTOMATIC_HIT)
        {
            // 50% chance of blocking ench-type effects at 10 displayed sh
            blocked = x_chance_in_y(SH, omnireflect_chance_denom(SH));

            dprf(DIAG_BEAM, "%smnireflected: %d/%d chance",
                 blocked ? "O" : "Not o", SH, omnireflect_chance_denom(SH));
        }
        else
        {
            // We use the original to-hit here.
            // (so that effects increasing dodge chance don't increase
            // block...?)
            const int testhit = random2(hit * 130 / 100);

            const int block = you.shield_bonus();

            dprf(DIAG_BEAM, "Beamshield: hit: %d, block %d", testhit, block);
            blocked = testhit < block;
        }

        // Divine shield only blocks conventionally blockable things, even if
        // the player is using the Warlock's Mirror.
        if (blocked || (you.duration[DUR_DIVINE_SHIELD] || is_blockable()))
        {
            const string refl_name = name.empty() &&
                                     origin_spell != SPELL_NO_SPELL ?
                                        spell_title(origin_spell) :
                                        name;

            const item_def *shield = you.shield();
            if (is_reflectable(you))
            {
                if (shield && shield_reflects(*shield))
                {
                    mprf("Your %s blocks the %s... and reflects it back!",
                            shield->name(DESC_PLAIN).c_str(),
                            refl_name.c_str());
                }
                else
                {
                    mprf("You block the %s... and reflect it back!",
                            refl_name.c_str());
                }
                reflect();
            }
            else
            {
                mprf("You block the %s.", name.c_str());
                finish_beam();
            }
            you.shield_block_succeeded(agent());

            // Use up a charge of Divine Shield, if active.
            tso_expend_divine_shield_charge();

            return true;
        }

        // Some training just for the "attempt".
        practise_shield_block(false);
    }

    if (is_enchantment())
        return false;

    if (!self_targeted())
        practise_being_shot_at();

    defer_rand r;

    const int repel = you.missile_repulsion() ? REPEL_MISSILES_EV_BONUS : 0;
    dodge += repel;

    const int hit_margin = _test_beam_hit(real_tohit, dodge, r);
    if (hit_margin < 0)
    {
        if (hit_margin > -repel)
        {
            mprf("The %s is repelled.", name.c_str());
            count_action(CACT_DODGE, DODGE_REPEL);
        }
        else
        {
            mprf("The %s misses you.", name.c_str());
            count_action(CACT_DODGE, DODGE_EVASION);
        }
    }
    else
        return false;

    return true;
}

void bolt::affect_player_enchantment(bool resistible)
{
    if (resistible
        && has_saving_throw()
        && you.check_willpower(agent(true), ench_power) > 0)
    {
        // You resisted it.

        // Give a message.
        bool need_msg = true;
        if (thrower != KILL_YOU_MISSILE)
        {
            const monster* mon = monster_by_mid(source_id);
            if (mon && !mon->observable())
            {
                mprf("Something tries to affect you, but you %s.",
                     you.willpower() == WILL_INVULN ? "are unaffected"
                                                   : "resist");
                need_msg = false;
            }
        }
        if (need_msg)
        {
            if (you.willpower() == WILL_INVULN)
                canned_msg(MSG_YOU_UNAFFECTED);
            else
            {
                // the message reflects the level of difficulty resisting.
                const int margin = you.willpower() - ench_power;
                mprf("You%s", you.resist_margin_phrase(margin).c_str());
            }
        }

        // punish the monster if we're a willful demon
        if (you.get_mutation_level(MUT_DEMONIC_WILL))
        {
            monster* mon = monster_by_mid(source_id);
            if (mon && mon->alive())
            {
                const int dam = 3 + random2(1 + div_rand_round(ench_power, 10));
                mprf("Your will lashes out at %s%s",
                     mon->name(DESC_THE).c_str(),
                     attack_strength_punctuation(dam).c_str());
                mon->hurt(&you, dam);
            }
        }

        // You *could* have gotten a free teleportation in the Abyss,
        // but no, you resisted.
        if (flavour == BEAM_TELEPORT && player_in_branch(BRANCH_ABYSS))
            xom_is_stimulated(200);

        extra_range_used += range_used_on_hit();
        return;
    }

    // Never affects the player.
    if (flavour == BEAM_INFESTATION
        || flavour == BEAM_ENFEEBLE)
    {
        return;
    }

    // You didn't resist it.
    if (animate)
        _ench_animation(effect_known ? real_flavour : BEAM_MAGIC);

    bool nasty = true, nice = false;

    const bool blame_player = god_cares() && YOU_KILL(thrower);

    switch (flavour)
    {
    case BEAM_HIBERNATION:
    case BEAM_SLEEP:
        you.put_to_sleep(nullptr, ench_power, flavour == BEAM_HIBERNATION);
        break;

    case BEAM_CORONA:
        you.backlight();
        obvious_effect = true;
        break;

    case BEAM_POLYMORPH:
        obvious_effect = you.polymorph(ench_power);
        break;

    case BEAM_MALMUTATE:
    case BEAM_UNRAVELLED_MAGIC:
        mpr("Strange energies course through your body.");
        you.malmutate(aux_source.empty() ? get_source_name() :
                      (get_source_name() + "/" + aux_source));
        obvious_effect = true;
        break;

    case BEAM_SLOW:
    case BEAM_SHADOW_TORPOR:
        slow_player(10 + random2(ench_power));
        obvious_effect = true;
        break;

    case BEAM_HASTE:
        haste_player(40 + random2(ench_power));
        did_god_conduct(DID_HASTY, 10, blame_player);
        obvious_effect = true;
        nasty = false;
        nice  = true;
        break;

    case BEAM_HEALING:
        potionlike_effect(POT_HEAL_WOUNDS, ench_power, true);
        obvious_effect = true;
        nasty = false;
        nice  = true;
        break;

    case BEAM_MIGHT:
        potionlike_effect(POT_MIGHT, ench_power);
        obvious_effect = true;
        nasty = false;
        nice  = true;
        break;

    case BEAM_INVISIBILITY:
        potionlike_effect(POT_INVISIBILITY, ench_power);
        obvious_effect = true;
        nasty = false;
        nice  = true;
        break;

    case BEAM_PARALYSIS:
        you.paralyse(agent(), random_range(2, 5));
        obvious_effect = true;
        break;

    case BEAM_PETRIFY:
        you.petrify(agent());
        obvious_effect = true;
        break;

    case BEAM_CONFUSION:
        confuse_player(5 + random2(3));
        obvious_effect = true;
        break;

    case BEAM_WEAKNESS:
        you.weaken(agent(), 8 + random2(4));
        obvious_effect = true;
        break;

    case BEAM_TELEPORT:
        you_teleport();

        // An enemy helping you escape while in the Abyss, or an
        // enemy stabilizing a teleport that was about to happen.
        if (!mons_att_wont_attack(attitude) && player_in_branch(BRANCH_ABYSS))
            xom_is_stimulated(200);

        obvious_effect = true;
        break;

    case BEAM_BLINK:
        uncontrolled_blink();
        obvious_effect = true;
        break;

    case BEAM_BLINK_CLOSE:
        blink_other_close(&you, source);
        obvious_effect = true;
        break;

    case BEAM_BECKONING:
        obvious_effect = beckon(you, *this);
        break;

    case BEAM_CHARM:
        mprf(MSGCH_WARN, "Your will is overpowered!");
        confuse_player(5 + random2(3));
        obvious_effect = true;
        break;     // charming - confusion?

    case BEAM_BANISH:
        if (YOU_KILL(thrower))
        {
            mpr("This spell isn't strong enough to banish yourself.");
            break;
        }
        you.banish(agent(), get_source_name(),
                   agent()->get_experience_level());
        obvious_effect = true;
        break;

    case BEAM_PAIN:
    {
        if (aux_source.empty())
            aux_source = "by nerve-wracking pain";

        const int dam = resist_adjust_damage(&you, flavour, damage.roll());
        if (dam)
        {
            mpr("Pain shoots through your body!");
            internal_ouch(dam);
            obvious_effect = true;
        }
        else
            canned_msg(MSG_YOU_UNAFFECTED);
        break;
    }

    case BEAM_AGONY:
        torment_player(agent(), TORMENT_AGONY);
        obvious_effect = true;
        break;

    case BEAM_DISPEL_UNDEAD:
        if (you.undead_state() == US_ALIVE)
        {
            canned_msg(MSG_YOU_UNAFFECTED);
            break;
        }

        mpr("You convulse!");

        if (aux_source.empty())
            aux_source = "by dispel undead";

        internal_ouch(damage.roll());
        obvious_effect = true;
        break;

    case BEAM_MINDBURST:
        mpr("Your mind is blasted!");

        if (aux_source.empty())
            aux_source = "mindburst bolt";

        {
            int amt = damage.roll();
            internal_ouch(amt);

            if (you.has_blood())
                blood_spray(you.pos(), MONS_PLAYER, amt / 5);
        }

        obvious_effect = true;
        break;

    case BEAM_PORKALATOR:
        if (!transform(ench_power, transformation::pig, true))
        {
            mpr("You feel a momentary urge to oink.");
            break;
        }

        you.transform_uncancellable = true;
        obvious_effect = true;
        break;

    case BEAM_BERSERK:
        you.go_berserk(blame_player);
        obvious_effect = true;
        break;

    case BEAM_SENTINEL_MARK:
        you.sentinel_mark();
        obvious_effect = true;
        break;

    case BEAM_DIMENSION_ANCHOR:
        mprf("You feel %sfirmly anchored in space.",
             you.duration[DUR_DIMENSION_ANCHOR] ? "more " : "");
        you.increase_duration(DUR_DIMENSION_ANCHOR, 12 + random2(15), 50);
        if (you.duration[DUR_TELEPORT])
        {
            you.duration[DUR_TELEPORT] = 0;
            mpr("Your teleport is interrupted.");
        }
        you.redraw_evasion = true;
        obvious_effect = true;
        break;

    case BEAM_VULNERABILITY:
        you.strip_willpower(agent(), 12 + random2(18));
        obvious_effect = true;
        break;

    case BEAM_VITRIFY:
        if (!you.duration[DUR_VITRIFIED])
            mpr("Your body becomes as fragile as glass!");
        else
            mpr("You feel your fragility will last longer.");
        you.increase_duration(DUR_VITRIFIED, random_range(8, 18), 50);
        obvious_effect = true;
        break;

    case BEAM_VITRIFYING_GAZE:
        if (!you.duration[DUR_VITRIFIED])
            mpr("Your body becomes as fragile as glass!");
        else
            mpr("You feel your fragility will last longer.");
        you.increase_duration(DUR_VITRIFIED, random_range(4, 8), 50);
        obvious_effect = true;
        break;

    case BEAM_MALIGN_OFFERING:
    {
        const int dam = resist_adjust_damage(&you, flavour, damage.roll());
        if (dam)
        {
            _malign_offering_effect(&you, agent(), dam);
            obvious_effect = true;
        }
        else
            canned_msg(MSG_YOU_UNAFFECTED);
        break;
    }

    case BEAM_ROOTS:
    {
        const int turns = 1 + random_range(div_rand_round(ench_power, 20),
                                           div_rand_round(ench_power, 12) + 1);
        if (start_ranged_constriction(*agent(), you, turns, CONSTRICT_ROOTS))
            obvious_effect = true;
        break;
    }

    case BEAM_VILE_CLUTCH:
    {
        const int turns = 3 + random_range(div_rand_round(ench_power, 50),
                                           div_rand_round(ench_power, 35) + 1);
        if (start_ranged_constriction(*agent(), you, turns, CONSTRICT_BVC))
            obvious_effect = true;
        break;
    }

    case BEAM_VAMPIRIC_DRAINING:
    {
        const int dam = resist_adjust_damage(&you, flavour, damage.roll());
        if (dam && actor_is_susceptible_to_vampirism(you))
        {
            _vampiric_draining_effect(you, *agent(), dam);
            obvious_effect = true;
        }
        else
            canned_msg(MSG_YOU_UNAFFECTED);
        break;
    }

    case BEAM_VIRULENCE:
        // Those completely immune cannot be made more susceptible this way
        if (you.res_poison(false) >= 3)
        {
            canned_msg(MSG_YOU_UNAFFECTED);
            break;
        }

        mpr("You feel yourself grow more vulnerable to poison.");
        you.increase_duration(DUR_POISON_VULN, 12 + random2(18), 50);
        obvious_effect = true;
        break;

    case BEAM_AGILITY:
        you.be_agile(ench_power);
        obvious_effect = true;
        nasty = false;
        nice  = true;
        break;

    case BEAM_SAP_MAGIC:
        mprf(MSGCH_WARN, "Your magic feels %stainted.",
             you.duration[DUR_SAP_MAGIC] ? "more " : "");
        you.increase_duration(DUR_SAP_MAGIC, random_range(20, 30), 50);
        break;

    case BEAM_DRAIN_MAGIC:
    {
        int amount = min(you.magic_points, random2avg(ench_power / 8, 3));
        if (!amount)
            break;
        mprf(MSGCH_WARN, "You feel your power leaking away.");
        drain_mp(amount);
        obvious_effect = true;
        break;
    }

    case BEAM_TUKIMAS_DANCE:
        cast_tukimas_dance(ench_power, &you);
        obvious_effect = true;
        break;

    case BEAM_RESISTANCE:
        potionlike_effect(POT_RESISTANCE, min(ench_power, 200));
        obvious_effect = true;
        nasty = false;
        nice  = true;
        break;

    case BEAM_UNRAVELLING:
        if (!player_is_debuffable())
            break;

        // If the player is unravelling themselves voluntarily, allow it to work.
        debuff_player(agent() && agent()->is_player());
        _unravelling_explode(*this);
        obvious_effect = true;
        break;

    case BEAM_SOUL_SPLINTER:
        obvious_effect = true;
        if (you.holiness() & (MH_NATURAL | MH_DEMONIC | MH_HOLY))
            make_soul_wisp(*agent(), you);
        else
            canned_msg(MSG_YOU_UNAFFECTED);
        break;

    default:
        // _All_ enchantments should be enumerated here!
        mpr("Software bugs nibble your toes!");
        break;
    }

    if (nasty)
    {
        if (mons_att_wont_attack(attitude))
        {
            friend_info.hurt++;
            if (source_id == MID_PLAYER)
            {
                // Beam from player rebounded and hit player.
                if (!self_targeted())
                    xom_is_stimulated(200);
            }
            else
            {
                // Beam from an ally or neutral.
                xom_is_stimulated(100);
            }
        }
        else
            foe_info.hurt++;
    }
    else if (nice)
    {
        if (mons_att_wont_attack(attitude))
            friend_info.helped++;
        else
        {
            foe_info.helped++;
            xom_is_stimulated(100);
        }
    }

    handle_petrify_chaining(you.pos());

    // Regardless of effect, we need to know if this is a stopper
    // or not - it seems all of the above are.
    extra_range_used += range_used_on_hit();
}

void bolt::affect_actor(actor *act)
{
    if (act->is_monster())
        affect_monster(act->as_monster());
    else
        affect_player();
}

struct pie_effect
{
    const char* desc;
    function<bool(const actor& def)> valid;
    function<void (actor& def, const bolt &beam)> effect;
    int weight;
};

static const vector<pie_effect> pie_effects = {
    {
        "plum",
        [](const actor &defender) {
            return defender.is_player();
        },
        [](actor &/*defender*/, const bolt &/*beam*/) {
            if (you.duration[DUR_VERTIGO])
                mpr("You feel your light-headedness will last longer.");
            else
                mpr("You feel light-headed.");

            you.increase_duration(DUR_VERTIGO, 10 + random2(11), 50);
        },
        10
    },
    {
        "lemon",
        [](const actor &defender) {
            return defender.is_player() && (you.can_drink()
                                            || you.duration[DUR_NO_POTIONS]); // allow stacking
        },
        [](actor &/*defender*/, const bolt &/*beam*/) {
            if (you.duration[DUR_NO_POTIONS])
                mpr("You feel your inability to drink will last longer.");
            else
                mpr("You feel unable to drink.");

            you.increase_duration(DUR_NO_POTIONS, 10 + random2(11), 50);
        },
        10
    },
    {
        "blueberry",
        nullptr,
        [](actor &defender, const bolt &beam) {
            if (defender.is_monster())
            {
                monster *mons = defender.as_monster();
                simple_monster_message(*mons, " loses the ability to speak.");
                mons->add_ench(mon_enchant(ENCH_MUTE, 0, beam.agent(),
                            4 + random2(7) * BASELINE_DELAY));
            }
            else
            {
                if (you.duration[DUR_SILENCE])
                    mpr("You feel your silence will last longer.");
                else
                    mpr("An unnatural silence engulfs you.");

                you.increase_duration(DUR_SILENCE, 4 + random2(7), 10);
                invalidate_agrid(true);

                if (you.beheld())
                    you.update_beholders();
            }
        },
        10
    },
    {
        "raspberry",
        [](const actor &defender) {
            return defender.is_player();
        },
        [](actor &/*defender*/, const bolt &/*beam*/) {
            for (int i = 0; i < NUM_STATS; ++i)
                lose_stat(static_cast<stat_type>(i), 1 + random2(3));
        },
        10
    },
    {
        "cherry",
        [](const actor &defender) {
            return defender.is_player() || defender.res_fire() < 3;
        },
        [](actor &defender, const bolt &beam) {
            if (defender.is_monster())
            {
                monster *mons = defender.as_monster();
                simple_monster_message(*mons,
                        " looks more vulnerable to fire.");
                mons->add_ench(mon_enchant(ENCH_FIRE_VULN, 0,
                             beam.agent(),
                             15 + random2(11) * BASELINE_DELAY));
            }
            else
            {
                if (you.duration[DUR_FIRE_VULN])
                {
                    mpr("You feel your vulnerability to fire will last "
                        "longer.");
                }
                else
                    mpr("Cherry-coloured flames burn away your fire "
                        "resistance!");

                you.increase_duration(DUR_FIRE_VULN, 15 + random2(11), 50);
            }
        },
        6
    },
    {
        "peanut brittle",
        nullptr,
        [](actor &defender, const bolt &beam) {
            if (defender.is_monster())
            {
                monster *mons = defender.as_monster();
                simple_monster_message(*mons,
                    " becomes as fragile as glass!");

                mons->add_ench(mon_enchant(ENCH_VITRIFIED, 0, beam.agent(),
                                           random_range(16, 36) * BASELINE_DELAY));
            }
            else
            {
                if (you.duration[DUR_VITRIFIED])
                    mpr("You feel your fragility will last longer.");
                else
                    mpr("Your body becomes as fragile as glass!");

                you.increase_duration(DUR_VITRIFIED, random_range(16, 36), 50);
            }
        },
        4
    },
    {
        "glitter",
        [](const actor &defender) {
            return defender.can_be_dazzled();
        },
        [](actor &defender, const bolt &beam) {
            if (defender.is_player())
                blind_player(random_range(16, 36), ETC_GLITTER);
            else
                dazzle_target(&defender, beam.agent(), 149);
        },
        5
    },
};

static pie_effect _random_pie_effect(const actor &defender)
{
    vector<pair<const pie_effect&, int>> weights;
    for (const pie_effect &effect : pie_effects)
        if (!effect.valid || effect.valid(defender))
            weights.push_back({effect, effect.weight});

    ASSERT(!weights.empty());

    return *random_choose_weighted(weights);
}

void bolt::affect_player()
{
    hit_count[MID_PLAYER]++;

    if (ignores_player())
        return;

    // If this is a friendly monster, firing a penetrating beam in the player's
    // direction, always stop immediately before them if this attack wouldn't
    // be harmless to them.
    if (agent() && agent()->is_monster() && mons_att_wont_attack(attitude)
        && !harmless_to_player() && pierce && !is_explosion)
    {
        ray.regress();
        finish_beam();
        return;
    }

    // Explosions only have an effect during their explosion phase.
    // Special cases can be handled here.
    if (is_explosion && !in_explosion_phase)
    {
        // Trigger the explosion.
        finish_beam();
        return;
    }

    if (is_tracer)
    {
        tracer_affect_player();
        return;
    }

    // Trigger an interrupt, so travel will stop on misses which
    // generate smoke.
    if (!YOU_KILL(thrower))
    {
        if (agent() && agent()->is_monster())
        {
            interrupt_activity(activity_interrupt::monster_attacks,
                               agent()->as_monster());
        }
        else
            interrupt_activity(activity_interrupt::monster_attacks);
    }

    if (flavour == BEAM_MISSILE && item)
    {
        ranged_attack attk(agent(true), &you, launcher,
                           item, use_target_as_pos,
                           agent(), item_mulches);
        attk.attack();
        // fsim purposes - throw_it detects if an attack connected through
        // hit_verb
        if (attk.ev_margin >= 0 && hit_verb.empty())
            hit_verb = attk.attack_verb;
        if (attk.reflected)
            reflect();
        extra_range_used += attk.range_used;
        return;
    }

    // Visible beams reveal invisible monsters; otherwise animations confer
    // an information advantage for sighted players
    if (visible() && agent() && agent()->is_monster())
        agent()->as_monster()->unseen_pos = agent()->pos();

    if (misses_player())
        return;

    if (!is_explosion && !noise_generated)
    {
        heard = noisy(loudness, pos(), source_id) || heard;
        noise_generated = true;
    }

    const bool engulfs = is_explosion || is_big_cloud();

    if (is_enchantment())
    {
        if (real_flavour == BEAM_CHAOS || real_flavour == BEAM_RANDOM)
        {
            if (hit_verb.empty())
                hit_verb = engulfs ? "engulfs" : "hits";
            mprf("The %s %s %s!", name.c_str(), hit_verb.c_str(),
                you.hp > 0 ? "you" : "your lifeless body");
        }

        affect_player_enchantment();
        return;
    }

    msg_generated = true;

    // FIXME: Lots of duplicated code here (compare handling of
    // monsters)
    int pre_ac_dam = 0;

    // Roll the damage.
    pre_ac_dam += damage.roll();
    int pre_res_dam = apply_AC(&you, pre_ac_dam);

#ifdef DEBUG_DIAGNOSTICS
    dprf(DIAG_BEAM, "Player damage: before AC=%d; after AC=%d",
                    pre_ac_dam, pre_res_dam);
#endif

    practise_being_shot();

    bool was_affected = false;
    int  old_hp       = you.hp;

    pre_res_dam = max(0, pre_res_dam);

    // If the beam is of the MMISSILE type (Earth magic) we might bleed on the
    // floor.
    if (!engulfs && flavour == BEAM_MMISSILE)
    {
        // assumes DVORP_PIERCING, factor: 0.5
        int blood = min(you.hp, pre_res_dam / 2);
        bleed_onto_floor(you.pos(), MONS_PLAYER, blood, true);
    }

    // Apply resistances to damage, but don't print "You resist" messages yet
    int final_dam = check_your_resists(pre_res_dam, flavour, "", this, false);

    // Tell the player the beam hit
    if (hit_verb.empty())
        hit_verb = engulfs ? "engulfs" : "hits";

    if (flavour != BEAM_VISUAL && !is_enchantment())
    {
        mprf("The %s %s %s%s%s", name.c_str(), hit_verb.c_str(),
             you.hp > 0 ? "you" : "your lifeless body",
             final_dam || damage.num == 0 ? "" : " but does no damage",
             attack_strength_punctuation(final_dam).c_str());
    }

    // Now print the messages associated with checking resistances, so that
    // these come after the beam actually hitting.
    // Note that this must be called with the pre-resistance damage, so that
    // poison effects etc work properly.
    check_your_resists(pre_res_dam, flavour, "", this, true);

    if (flavour == BEAM_STUN_BOLT
        && !you.duration[DUR_PARALYSIS]
        && (you.res_elec() <= 0 || coinflip()))
    {
        const bool was_parad = you.duration[DUR_PARALYSIS];
        you.paralyse(agent(), 1);
        was_affected = (!was_parad && you.duration[DUR_PARALYSIS]) || was_affected;

    }

    if (flavour == BEAM_LIGHT && you.can_be_dazzled())
        blind_player(random_range(7, 12), WHITE);

    if (flavour == BEAM_MIASMA && final_dam > 0)
        was_affected = miasma_player(agent(), name);

    if (flavour == BEAM_DESTRUCTION) // MINDBURST already handled
        blood_spray(you.pos(), MONS_PLAYER, final_dam / 5);

    // Confusion effect for spore explosions
    if (flavour == BEAM_SPORE && final_dam
        && !(you.holiness() & MH_UNDEAD)
        && !you.is_unbreathing())
    {
        confuse_player(2 + random2(3));
    }

    if (flavour == BEAM_UNRAVELLED_MAGIC)
        affect_player_enchantment();

    // Sticky flame.
    if (origin_spell == SPELL_STICKY_FLAME
        || flavour == BEAM_STICKY_FLAME)
    {
        // ench_power here is equal to 12 * caster HD for monsters, btw
        const int intensity = 2 + ench_power / 14;

        if (!player_res_sticky_flame())
        {
            sticky_flame_player(intensity, random_range(11, 21),
                                get_source_name(), aux_source);
            was_affected = true;
        }
    }

    // need to trigger qaz resists after reducing damage from ac/resists.
    //    for some reason, strength 2 is the standard. This leads to qaz's
    //    resists triggering 2 in 5 times at max piety.
    //    perhaps this should scale with damage?
    // what to do for hybrid damage?  E.g. bolt of magma, icicle, poison arrow?
    // Right now just ignore the physical component.
    // what about acid?
    you.expose_to_element(flavour, 2, false);

    // Manticore spikes
    if (origin_spell == SPELL_THROW_BARBS && final_dam > 0)
        barb_player(random_range(4, 8), 4);

    if (origin_spell == SPELL_GRAVE_CLAW)
    {
        mpr("You are skewered in place!");
        you.increase_duration(DUR_NO_MOMENTUM, random_range(2, 4));
    }

    if (flavour == BEAM_ENSNARE)
        was_affected = ensnare(&you) || was_affected;

    if (origin_spell == SPELL_QUICKSILVER_BOLT)
        debuff_player();

    if (origin_spell == SPELL_NULLIFYING_BREATH)
    {
        debuff_player();
        int amount = min(you.magic_points, random2avg(ench_power, 3));
        if (amount > 0)
        {
            mprf(MSGCH_WARN, "You feel your power leaking away.");
            drain_mp(amount);
        }
    }

    if (origin_spell == SPELL_THROW_PIE && final_dam > 0)
    {
        const pie_effect effect = _random_pie_effect(you);
        mprf("%s!", effect.desc);
        effect.effect(you, *this);
    }

    dprf(DIAG_BEAM, "Damage: %d", final_dam);

    if (final_dam > 0 || old_hp < you.hp || was_affected)
    {
        if (mons_att_wont_attack(attitude))
        {
            friend_info.hurt++;

            // Beam from player rebounded and hit player.
            // Xom's amusement at the player's being damaged is handled
            // elsewhere.
            if (source_id == MID_PLAYER)
            {
                if (!self_targeted())
                    xom_is_stimulated(200);
            }
            else if (was_affected)
                xom_is_stimulated(100);
        }
        else
            foe_info.hurt++;
    }

    internal_ouch(final_dam);

    // Acid. (Apply this afterward, to avoid bad message ordering.)
    if (flavour == BEAM_ACID)
        you.acid_corrode(5);

    extra_range_used += range_used_on_hit();

    knockback_actor(&you, final_dam);
    pull_actor(&you, final_dam);

    if (origin_spell == SPELL_FLASH_FREEZE
        || origin_spell == SPELL_CREEPING_FROST
        || name == "blast of ice"
        || origin_spell == SPELL_HOARFROST_BULLET
        || origin_spell == SPELL_GLACIATE && !is_explosion)
    {
        if (!you.duration[DUR_FROZEN])
        {
            mprf(MSGCH_WARN, "You are encased in ice.");
            you.duration[DUR_FROZEN] = (random_range(3, 5)) * BASELINE_DELAY;
        }
    }
}

bool bolt::ignores_player() const
{
    // Digging -- don't care.
    if (flavour == BEAM_DIGGING)
        return true;

    if (origin_spell == SPELL_COMBUSTION_BREATH
        || origin_spell == SPELL_NULLIFYING_BREATH
        || origin_spell == SPELL_RIMEBLIGHT
        || origin_spell == SPELL_SHADOW_PRISM)
    {
        return true;
    }

    if (origin_spell == SPELL_HOARFROST_BULLET && is_explosion
        && agent() && agent()->wont_attack())
    {
        return true;
    }

    if (agent() && agent()->is_monster()
        && (mons_is_hepliaklqana_ancestor(agent()->as_monster()->type)
            || mons_is_player_shadow(*agent()->as_monster())
            || agent()->real_attitude() == ATT_MARIONETTE))
    {
        // friends!
        return true;
    }

    if (source_id == MID_PLAYER_SHADOW_DUMMY)
        return true;

    if (flavour == BEAM_ROOTS || flavour == BEAM_VILE_CLUTCH)
    {
        return agent()->wont_attack()
               || !agent()->can_constrict(you, flav
